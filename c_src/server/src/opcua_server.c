/*----------------------------------------------------------------
* Copyright (c) 2021 Faceplate
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
#include <open62541/plugin/log_stdout.h>
#include <open62541/server.h>
#include <open62541/server_config_default.h>
//----------------------------------------
#include <openssl/x509v3.h>
#include <openssl/bn.h>
#include <openssl/asn1.h>
#include <openssl/x509.h>
#include <openssl/x509_vfy.h>
#include <openssl/pem.h>
#include <openssl/bio.h>
//----------------------------------------
#include "utilities.h"

//------------test------------------------
#include "opcua_server_test.h"

//-----------command handlers-------------
cJSON* on_request( char *method, cJSON *args, char **error );

cJSON* opcua_server_start(cJSON* args, char **error);
cJSON* opcua_server_add_nodes(cJSON* args, char **error);
cJSON *add_node(cJSON* args, char **error);

//---------server thread entry point------
static void *server_thread(void* arg);

//------------utilities-----------------------
char** str_split(char* a_str, const char a_delim);

// The RUN flag
static volatile UA_Boolean running = true;
UA_Server *server = NULL;

//-------------------The command loop-------------------------------------
cJSON* on_request( char *method, cJSON *args, char **error ){
    
    cJSON *response = NULL;
    // Handle the request
    LOGDEBUG("handle the request %s", method);
    if( strcmp(method, "start") == 0){
        response = opcua_server_start( args, error );
    } else{
        *error = "invalid method";
    }

    return response;
}

//--------------------command handlers----------------------------------
cJSON* opcua_server_start(cJSON* args, char **error){
    cJSON *response = NULL;

    LOGINFO("launching a server thread");

    if (server != NULL){
        *error = "server is already run";
        goto on_error;
    }

    // Create a new server instance
    server = UA_Server_new();

    // The server is going to run in a dedicated thread
    pthread_t serverThread;

    // Configure the server
    UA_ServerConfig_setDefault(UA_Server_getConfig(server));

    // Launch the server thread
    int res = pthread_create( &serverThread, NULL, &server_thread, server);

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


cJSON* opcua_server_add_nodes(cJSON* args, char **error) {
    cJSON *response = NULL;

    return response;
on_error:
    cJSON_Delete( response );
    return NULL;
}

cJSON *add_node(cJSON* args, char **error){

    cJSON *path = cJSON_GetObjectItemCaseSensitive(args, "path");
    cJSON *type = cJSON_GetObjectItemCaseSensitive(args, "type");

    /* Define the attrsibute of the myInteger variable node */
    UA_VariableAttributes attr = UA_VariableAttributes_default;
    UA_Int32 myInteger = 42;
    UA_Variant_setScalar(&attr.value, &myInteger, &UA_TYPES[UA_TYPES_INT32]);
    attr.description = UA_LOCALIZEDTEXT("en-US","the answer");
    attr.displayName = UA_LOCALIZEDTEXT("en-US","the answer");
    attr.dataType = UA_TYPES[UA_TYPES_INT32].typeId;
    attr.accessLevel = UA_ACCESSLEVELMASK_READ | UA_ACCESSLEVELMASK_WRITE;

    /* Add the variable node to the information model */
    UA_NodeId myIntegerNodeId = UA_NODEID_STRING(1, "the.answer");
    UA_QualifiedName myIntegerName = UA_QUALIFIEDNAME(1, "the answer");
    UA_NodeId parentNodeId = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
    UA_NodeId parentReferenceNodeId = UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES);
    UA_Server_addVariableNode(server, myIntegerNodeId, parentNodeId,
                              parentReferenceNodeId, myIntegerName,
                              UA_NODEID_NUMERIC(0, UA_NS0ID_BASEDATAVARIABLETYPE), attr, NULL, NULL);
    
    return cJSON_CreateString("ok");

// error:
//     return result;
}

//---------------The server thread-------------------------------------------
static void *server_thread(void *arg) {
    UA_Server * server = arg;
    LOGINFO("starting the server");

    // The returns only when the server shuts down
    UA_StatusCode retval = UA_Server_run(server, &running);

    UA_Server_delete(server);

    char *status = (char *)UA_StatusCode_name( retval );
    if (retval != UA_STATUSCODE_GOOD){
        LOGERROR("unable to start server, status %s", status);
    }

    // Set the flag to the ready state
    running = true;
    server = NULL;

    return status;
}

//------------------------catch signals------------------------------------------------
static void stopHandler(int sig) {
    LOGINFO("received ctrl-c");
    running = false;
}

//------------------------THE ENTRY POINT------------------------------------------------
int main(int argc, char *argv[]) {

    signal(SIGINT, stopHandler);
    signal(SIGTERM, stopHandler);

    OpenSSL_add_all_algorithms();
    ERR_load_BIO_strings();

    SETLOGLEVEL(0);

    // LOGINFO("enter eport_loop");
    // eport_loop( &on_request );
    //testStartServer();

    test_server_start();
    sleep(2);
    discovery_test("opc.tcp://localhost:4840");
    add_simple_node_test("TAGS/My folder/AI", "Double", cJSON_CreateNumber(56.0));
    sleep(600);

    test_server_stop();

    return EXIT_SUCCESS;
}
