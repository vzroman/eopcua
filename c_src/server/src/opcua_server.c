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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
//----------------------------------------
#include <eport_c.h>
//----------------------------------------
#include <open62541/server.h>
//----------------------------------------
#include <openssl/x509v3.h>
#include <openssl/bn.h>
#include <openssl/asn1.h>
#include <openssl/x509.h>
#include <openssl/x509_vfy.h>
#include <openssl/pem.h>
#include <openssl/bio.h>
//----------------------------------------
#include "opcua_server.h"
#include "utilities.h"
#include "opcua_server_config.h"

//------------test------------------------
#include "opcua_server_test.h"

//-----------command handlers-------------
cJSON* on_request( char *method, cJSON *args, char **error );

cJSON* opcua_server_start(cJSON* args, char **error);
cJSON* opcua_server_write_items(cJSON* args, char **error);
cJSON* opcua_server_write_item(cJSON* args, char **error);
cJSON *add_node(cJSON* args, char **error);

//-----------path mapping-----------------
opcua_server_mapping *opcua_server_map = NULL;

//---------server thread entry point------
static void *server_thread(void* arg);
// The RUN flag
static volatile UA_Boolean opcua_server_running = false;
UA_Server *opcua_server = NULL;

//-------------local functions-------------------
char *create_variable(char *path, char *type, UA_NodeId *node);
char *ensure_path(char **path, int depth,  UA_NodeId folder, UA_NodeId *node);
char *find_in_folder( const UA_NodeId folder, const char *name, UA_NodeId *nodeId );
char *create_folder( const UA_NodeId folder, const char *name, UA_NodeId *nodeId );


//-------------------The command loop-------------------------------------
cJSON* on_request( char *method, cJSON *args, char **error ){
    
    cJSON *response = NULL;
    // Handle the request
    LOGTRACE("handle the request %s", method);

    if( strcmp(method, "server_start") == 0){
        response = opcua_server_start( args, error );
    } else if( strcmp(method, "write_items") == 0){
        response = opcua_server_write_items( args, error );
    }else if( strcmp(method, "write_item") == 0){
        response = opcua_server_write_item( args, error );
    } else{
        *error = "invalid method";
    }

    return response;
}

//--------------------command handlers----------------------------------
cJSON* opcua_server_start(cJSON* args, char **error){
    cJSON *response = NULL;

    if (opcua_server != NULL){
        *error = "server is already run";
        goto on_error;
    }

    // Create a new server instance
    opcua_server = UA_Server_new();
    UA_ServerConfig *config = UA_Server_getConfig( opcua_server );

    // Configure the server accordingly to the arguments
    *error = configure(config, args);
    if (*error != NULL) goto on_error;

    // The server is going to run in a dedicated thread
    pthread_t serverThread;

    // Launch the server thread
    int res = pthread_create( &serverThread, NULL, &server_thread, NULL);

    if (res !=0 ){
        *error = "unable to launch the server thread";
        goto on_error;
    }

    // the server has started
    return cJSON_CreateString("ok");

on_error:
    cJSON_Delete( response );
    return NULL;

}



cJSON* opcua_server_write_items(cJSON* args, char **error){
    LOGTRACE("write items");
    cJSON *response = cJSON_CreateArray();
    cJSON *item = NULL;
    cJSON *result = NULL;

    if (opcua_server == NULL){
        *error = "server not started";
        goto on_error;
    }

    //-----------validate the arguments-----------------------
    if ( !cJSON_IsArray(args) ) {
        *error = "invalid write arguments";
        goto on_error;
    }

    cJSON_ArrayForEach(item, args) {
        result = opcua_server_write_item( item, error );
        if (result == NULL){
            result = cJSON_CreateString("error: write error");
        }
        if ( !cJSON_AddItemToArray(response, result) ){
            *error = "unable add a result for item";
            goto on_error;
        }
    }

    return response;
on_error:
    cJSON_Delete( response );
    return NULL;
}

cJSON* opcua_server_write_item(cJSON* args, char **error){
    LOGTRACE("write item");

    UA_StatusCode sc;

    if (opcua_server == NULL){
        *error = "server not started";
        goto on_error;
    }

    //-----------validate the arguments-----------------------
    cJSON *path = cJSON_GetObjectItemCaseSensitive(args, "path");
    if (!cJSON_IsString(path) || (path->valuestring == NULL)){
        *error = "path is not defined";
        goto on_error; 
    }

    cJSON *type = cJSON_GetObjectItemCaseSensitive(args, "type");
    if (!cJSON_IsString(type) || (type->valuestring == NULL)){
        *error = "type is not defined";
        goto on_error; 
    }

    cJSON *value = cJSON_GetObjectItemCaseSensitive(args, "value");
    if (value == NULL){
        *error = "value is not defined";
        goto on_error; 
    }

    // get item path
    char *_path = path->valuestring;

    // Lookup the binding in the collection
    opcua_server_mapping *m = NULL;
    HASH_FIND_STR(opcua_server_map, _path, m);

    if (m == NULL){
        // The binding is not in the collection yet.
        // Create a new subscription.
        LOGDEBUG("create a new node %s", _path);

        UA_NodeId nodeId;
        *error = create_variable(_path, type->valuestring, &nodeId);
        if (*error) goto on_error;

        // Add the node to the collection
        m = (opcua_server_mapping *)malloc(sizeof *m);
        if (m == NULL){
            *error = "out of memory";
            goto on_error;
        }
        m->path = strdup( _path );
        sc = UA_NodeId_copy(&nodeId, &m->nodeId );
        if (sc != UA_STATUSCODE_GOOD){
            *error = (char*)UA_StatusCode_name( sc );
            goto on_error;
        }

        HASH_ADD_STR(opcua_server_map, path, m);
        
        return opcua_server_write_item(args, error);
        
    }else{
        // The binding is already in the active subscriptions
        LOGTRACE("write value");

        if (cJSON_IsNull(value)){
            // NULL reset the value, bad status
            UA_WriteValue wv;
            UA_WriteValue_init(&wv);
            wv.nodeId = m->nodeId;
            wv.attributeId = UA_ATTRIBUTEID_VALUE;
            wv.value.status = UA_STATUSCODE_BADNOTCONNECTED;
            wv.value.hasStatus = true;
            sc = UA_Server_write(opcua_server, &wv);
            if (sc != UA_STATUSCODE_GOOD){
                *error = (char*)UA_StatusCode_name( sc );
                goto on_error;
            }
        }else{
            const UA_DataType *ua_type = type2ua( type->valuestring );
            if (ua_type == NULL){
                *error = "unsupported data type";
                goto on_error;
            }

            UA_Variant *ua_value = json2ua(ua_type, value);
            if (ua_value == NULL){
                *error = "invalid value";
                goto on_error;
            }
            sc = UA_Server_writeValue(opcua_server, m->nodeId, *ua_value);
            UA_Variant_delete(ua_value);
            if (sc != UA_STATUSCODE_GOOD){
                *error = (char*)UA_StatusCode_name( sc );
                goto on_error;
            }
        }
    }

    return cJSON_CreateString("ok");

on_error:
    return NULL;
}

char *create_variable(char *path, char *type, UA_NodeId *node) {
    char *error = NULL;
    UA_StatusCode sc;

    const UA_DataType *ua_type = type2ua( type );
    if (ua_type == NULL){
        error = "unsupported data type";
        goto on_error;
    }

    char *name = NULL;
    UA_NodeId folder = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
    UA_NodeId referenceTypeId = UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES);
    char **tokens = str_split( path, '/');
    if (tokens){
        int depth = 0;
        while (*(tokens + depth)) depth++;
        error = ensure_path(tokens, depth-1, folder, &folder);
        if (error != NULL) {
            goto on_error;
        }

        referenceTypeId = UA_NODEID_NUMERIC(0, UA_NS0ID_HASCOMPONENT);
        name = strdup(*(tokens + depth -1));
        str_split_destroy( tokens );
        tokens = NULL;
    }else{
        name = strdup( path );
    }
    
    UA_VariableAttributes attr = UA_VariableAttributes_default;
    attr.accessLevel = UA_ACCESSLEVELMASK_READ | UA_ACCESSLEVELMASK_WRITE;
    attr.displayName = UA_LOCALIZEDTEXT_ALLOC("en-US", name);
    attr.dataType = ua_type->typeId;
    UA_QualifiedName qname = UA_QUALIFIEDNAME_ALLOC(1, name);

    free(name);
    name = NULL;

    sc = UA_Server_addVariableNode(opcua_server, 
        UA_NODEID_NULL, 
        folder, 
        referenceTypeId,
        qname,
        UA_NODEID_NUMERIC(0, UA_NS0ID_BASEDATAVARIABLETYPE), 
        attr, 
        NULL, 
        node);
    
    if (sc != UA_STATUSCODE_GOOD){
        error = (char*)UA_StatusCode_name( sc );
        goto on_error;
    }

on_error:
    return error;
}

char *ensure_path(char **path, int depth,  UA_NodeId folder, UA_NodeId *node) {

    char *error = NULL;

    for (int i = 0; i < depth; i++){
        char *name = *(path + i);
        error = find_in_folder( *node, name, node );
        if (error != NULL) {
            if (strcmp(error,"not found") == 0){
                error = create_folder(*node, name, node);
                if (error != NULL) break;
            }else { 
                break; 
            }
        }
    }

    return error;
}

char *find_in_folder( const UA_NodeId folder, const char *name, UA_NodeId *nodeId ){
    
    char *error = NULL;
    UA_StatusCode sc;
    
    UA_QualifiedName qname = UA_QUALIFIEDNAME(1, (char *)name);
    UA_BrowsePathResult result = UA_Server_browseSimplifiedBrowsePath(opcua_server, folder, 1, &qname);

    if (result.statusCode == UA_STATUSCODE_GOOD){
        if (result.targetsSize == 1 ){
            sc = UA_NodeId_copy( &result.targets[0].targetId.nodeId, nodeId);
            if (sc != UA_STATUSCODE_GOOD) {
                error = (char*)UA_StatusCode_name( sc );
            }
        }else{
            // Status is good but the node is not found, is it a case?
            error = "not found";
        }
    }else if(result.statusCode == UA_STATUSCODE_BADNOMATCH){
        // Node is not found
        error = "not found";
    }else{
        // Other error
        error = (char*)UA_StatusCode_name( result.statusCode );
    }

    UA_BrowsePathResult_clear( &result );
    return error;
}

char *create_folder( const UA_NodeId folder, const char *name, UA_NodeId *nodeId ){
    char *error = NULL;
    UA_StatusCode sc;

    UA_NodeId referenceTypeId;
    UA_NodeId root = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);

    if (UA_NodeId_equal(&folder,&root)){
        referenceTypeId = UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES);
    }else{
        referenceTypeId = UA_NODEID_NUMERIC(0, UA_NS0ID_HASCOMPONENT);
    }

    UA_ObjectAttributes attr = UA_ObjectAttributes_default;
    attr.displayName = UA_LOCALIZEDTEXT("en-US", (char *)name);
    UA_QualifiedName qname = UA_QUALIFIEDNAME(1, (char *)name);

    sc = UA_Server_addObjectNode(
        opcua_server, 
        UA_NODEID_NULL,
        folder,
        referenceTypeId,
        qname, 
        UA_NODEID_NUMERIC(0, UA_NS0ID_BASEOBJECTTYPE),
        attr, 
        NULL, 
        nodeId);

    if (sc != UA_STATUSCODE_GOOD){
        error = (char*)UA_StatusCode_name( sc );
    }
    
    return error;

}

//---------------The server thread-------------------------------------------
static void *server_thread(void *arg) {
    LOGINFO("starting the server thread");

    // The returns only when the server shuts down
    opcua_server_running = true;
    UA_StatusCode retval = UA_Server_run(opcua_server, &opcua_server_running);

    // Clean up
    UA_Server_delete(opcua_server);
    opcua_server = NULL;
    opcua_server_running = true;

    char *status = (char *)UA_StatusCode_name( retval );
    if (retval != UA_STATUSCODE_GOOD){
        LOGERROR("unable to start server, status %s", status);
    }

    // Set the flag to the ready state
    return status;
}

//------------------------catch signals------------------------------------------------
static void stopHandler(int sig) {
    LOGINFO("received ctrl-c");
    opcua_server_running = false;
}

//------------------------THE ENTRY POINT------------------------------------------------
int main(int argc, char *argv[]) {

    signal(SIGINT, stopHandler);
    signal(SIGTERM, stopHandler);

    OpenSSL_add_all_algorithms();
    ERR_load_BIO_strings();

    SETLOGLEVEL(0);

    LOGINFO("enter eport_loop");
    eport_loop( &on_request );

    // test_server_start();
    // sleep(2);
    // discovery_test("opc.tcp://localhost:4840");
    // add_simple_node_test("TAGS/My folder/AI", "Double", cJSON_CreateNumber(56.0));
    // sleep(600);

    // test_server_stop();

    return EXIT_SUCCESS;
}
