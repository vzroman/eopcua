/*----------------------------------------------------------------
* Copyright (c) 2022 Faceplate
*
* This file is provided to you under the Apache License,
* Version 2.0 (the "License"); you may not use this file
* except in compliance with the License.  You may obtain
* a copy of the License at
*
*   http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied.  See the License for the
* specific language governing permissions and limitations
* under the License.
----------------------------------------------------------------*/
//----------------------------------------
#include <pthread.h>
//----------------------------------------
#include <open62541/server.h>
//----------------------------------------
#include "opcua_server_config.h"
#include "opcua_server_loop.h"
#include "opcua_server_nodes.h"
#include "utilities.h"

struct OPCUA_SERVER {
  UA_Server *server;
  pthread_mutex_t lock;
  UA_Boolean run;
} opcua_server;

//---------------The server thread-------------------------------------------
static void *server_thread(void *arg) {
    LOGINFO("starting the server thread");

    UA_Boolean waitInternal = false;

    UA_StatusCode sc = UA_Server_run_startup( opcua_server.server );
    if(sc != UA_STATUSCODE_GOOD)
        goto on_clean;
    
    opcua_server.run = true;
    while( opcua_server.run ) {

        // get the lock
        pthread_mutex_lock(&opcua_server.lock);

        UA_UInt16 timeout = UA_Server_run_iterate(opcua_server.server, waitInternal);

        // release the lock
        pthread_mutex_unlock(&opcua_server.lock);

        struct timeval tv;
        tv.tv_sec = 0;
        tv.tv_usec = timeout * 1000;
        select(0, NULL, NULL, NULL, &tv);
    }
    sc = UA_Server_run_shutdown( opcua_server.server );

    // Clean up
on_clean:
    UA_Server_delete(opcua_server.server);
    opcua_server.server = NULL;
    opcua_server.run = false;

    pthread_mutex_destroy(&opcua_server.lock);

    char *status = (char *)UA_StatusCode_name( sc );
    if (sc != UA_STATUSCODE_GOOD){
        LOGERROR("unable to start server, status %s", status);
    }

    // Set the flag to the ready state
    return status;
}

char* start(cJSON *args){
    char *error = NULL;
    UA_ServerConfig *config = NULL;

    if (opcua_server.run) return "already started";

    // Create a new server instance
    opcua_server.server = UA_Server_new();
    if (!opcua_server.server) return "unable to allocate the connection object";

    config = UA_Server_getConfig( opcua_server.server );

    // Configure the server accordingly to the arguments
    error = configure(config, args);
    if (error) goto on_error;

    // The server is going to run in a dedicated thread
    pthread_t serverThread;

    if (pthread_mutex_init(&opcua_server.lock, NULL)) {
        error = "mutex init has failed";
        goto on_error;
    }

    // Launch the server thread
    int res = pthread_create( &serverThread, NULL, &server_thread, NULL);

    if (res !=0 ){
        error = "unable to launch the server thread";
        pthread_mutex_destroy(&opcua_server.lock);
        goto on_error;
    }

    // the server has started
    return NULL;

on_error:
    if (opcua_server.server) {
        UA_Server_delete(opcua_server.server);
    }

    opcua_server.server = NULL;
    opcua_server.run = false;
    if(config) UA_ServerConfig_clean(config);
    return error;

}

void stop(){
    purge_nodes();
    opcua_server.run = false;
}

bool is_started(){
    return opcua_server.run;
}

char *add_variable(UA_NodeId folder, char *name, UA_NodeId *outNodeId){
    
    UA_VariableAttributes attr = UA_VariableAttributes_default;
    attr.accessLevel = UA_ACCESSLEVELMASK_READ | UA_ACCESSLEVELMASK_WRITE;
    attr.displayName = UA_LOCALIZEDTEXT_ALLOC("en-US", name);
    //attr.dataType = type->typeId;
    UA_QualifiedName qname = UA_QUALIFIEDNAME_ALLOC(1, name);

    // open62541 is not thread safe, we use mutex
    pthread_mutex_lock(&opcua_server.lock); 

    UA_StatusCode sc = UA_Server_addVariableNode(
        opcua_server.server, 
        UA_NODEID_NULL, 
        folder, 
        UA_NODEID_NUMERIC(0, UA_NS0ID_HASCOMPONENT),
        qname,
        UA_NODEID_NUMERIC(0, UA_NS0ID_BASEDATAVARIABLETYPE), 
        attr, 
        NULL, 
        outNodeId
    );

    // release the lock
    pthread_mutex_unlock(&opcua_server.lock);

    //TODO?
    //UA_QualifiedName_clear( &qname );
    
    if (sc != UA_STATUSCODE_GOOD){
        return (char*)UA_StatusCode_name( sc );
    }else{
        return NULL;
    }
}

char *add_folder(UA_NodeId folder, char *name, UA_NodeId *outNodeId){

    UA_ObjectAttributes attr = UA_ObjectAttributes_default;
    attr.displayName = UA_LOCALIZEDTEXT_ALLOC("en-US", (char *)name);
    UA_QualifiedName qname = UA_QUALIFIEDNAME_ALLOC(1, (char *)name);

    // open62541 is not thread safe, we use mutex
    pthread_mutex_lock(&opcua_server.lock);

    UA_StatusCode sc = UA_Server_addObjectNode(
        opcua_server.server, 
        UA_NODEID_NULL,
        folder,
        UA_NODEID_NUMERIC(0, UA_NS0ID_HASCOMPONENT),
        qname, 
        UA_NODEID_NUMERIC(0, UA_NS0ID_FOLDERTYPE),
        attr, 
        NULL, 
        outNodeId
    );

    // release the lock
    pthread_mutex_unlock(&opcua_server.lock);

    if (sc != UA_STATUSCODE_GOOD) return (char*)UA_StatusCode_name( sc );

    return NULL;
}

char *write_value(UA_NodeId *nodeId, char *type, cJSON *value){
    UA_StatusCode sc;

    if (cJSON_IsNull(value)){

        // NULL reset the value, bad status
        UA_WriteValue wv;
        UA_WriteValue_init(&wv);
        wv.nodeId = *nodeId;
        wv.attributeId = UA_ATTRIBUTEID_VALUE;
        wv.value.status = UA_STATUSCODE_BADNOTCONNECTED;
        wv.value.hasStatus = true;

        pthread_mutex_lock(&opcua_server.lock);
        sc = UA_Server_write(opcua_server.server, &wv);
        pthread_mutex_unlock(&opcua_server.lock);

        if (sc != UA_STATUSCODE_GOOD){
            return (char*)UA_StatusCode_name( sc );
        }
    }else{
        const UA_DataType *ua_type = type2ua( type );
        if (!ua_type) return "unsupported data type";

        UA_Variant *ua_value = json2ua(ua_type, value);
        if (!ua_value) return "invalid value";

        pthread_mutex_lock(&opcua_server.lock);
        sc = UA_Server_writeValue(opcua_server.server, *nodeId, *ua_value);
        pthread_mutex_unlock(&opcua_server.lock);
        UA_Variant_delete(ua_value);

        if (sc != UA_STATUSCODE_GOOD){
            return (char*)UA_StatusCode_name( sc );
        }
    }

    return NULL;
}

char *read_value(UA_NodeId *nodeId, cJSON **value){

    UA_Variant ua_value;
    pthread_mutex_lock(&opcua_server.lock);
    UA_StatusCode sc = UA_Server_readValue(opcua_server.server, *nodeId, &ua_value);
    pthread_mutex_unlock(&opcua_server.lock);

    if (sc != UA_STATUSCODE_GOOD ) {
        return (char*)UA_StatusCode_name( sc );
    }

    *value = ua2json( ua_value.type, ua_value.data );
    if (!*value ) return "data type is no supported";

    return NULL;
}
