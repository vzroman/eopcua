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
#include "opcua_server.h"
#include "opcua_server_protocol.h"

//-----------helpers----------------------
cJSON* on_error(char* text);
cJSON* on_ok(cJSON* response);

//-----------command handlers-------------
cJSON* opcua_server_start(cJSON* request);
cJSON* opcua_server_add_nodes(cJSON* request);

//---------server thread entry point------
static void *startServer(void* arg);
char *add_node(cJSON* node);

//------------utilities-----------------------
char** str_split(char* a_str, const char a_delim);

// The RUN flag
static volatile UA_Boolean running = true;
UA_Server *server = NULL;

//-------------------The command loop-------------------------------------
char* on_request( char *requestString ){
    cJSON *response;
    char *responseString;
    OPCUA_SERVER_REQUEST request = {};

    // Parse the request
    LOGDEBUG("DEBUG: parsing the request\r\n");
    if (parse_request( requestString, &request ) != 0){
        response = on_error("invalid request");
    }else{
        // Handle the request
        LOGDEBUG("DEBUG: handle the request\r\n");
        if( request.cmd == OPCUA_SERVER_START ){
            response = opcua_server_start( request.body );
        } else{
            response = on_error("unsupported command type");
        }
    }

    // Reply (purges the response)
    responseString = create_response( &request, response );

    purge_request( &request );
    LOGDEBUG("DEBUG: response %s\r\n",responseString);

    return responseString;
}

//-------------------helpers-------------------------------------
cJSON* on_error(char* text){
    cJSON *response = cJSON_CreateObject();
    if (cJSON_AddStringToObject(response, "type", "error") == NULL) {
        goto error;
    }
    if (cJSON_AddStringToObject(response, "text", text) == NULL) {
        goto error;
    }
    return response;

error:
    cJSON_Delete( response );
    return NULL;
}

cJSON* on_ok(cJSON *result){
    cJSON *response = cJSON_CreateObject();
    if ( cJSON_AddStringToObject(response, "type", "ok") == NULL) {
        goto error;
    }
    if ( !cJSON_AddItemToObject(response, "result", result) ) {
        goto error;
    }
    return response;

error:
    cJSON_Delete( response );
    return NULL;
}

//--------------------command handlers----------------------------------
cJSON* opcua_server_start(cJSON* request){
    cJSON *response = NULL;
    char *errorString = NULL;

    if (server != NULL){
        errorString = "server is already run";
        goto error;
    }

    // Create a new server instance
    server = UA_Server_new();

    // The server is going to run in a dedicated thread
    pthread_t serverThread;

    // Configure the server
    UA_ServerConfig_setDefault(UA_Server_getConfig(server));

    // Launch the server thread
    int res = pthread_create( &serverThread, NULL, &startServer, server);

    if (res !=0 ){
        errorString = "unable to launch the server thread";
        goto error;
    }

    // the server has started
    response = cJSON_CreateString("ok");
    return on_ok( response );

error:
    cJSON_Delete( response );
    if (errorString == NULL){
        errorString = "programming error in opcua_server_start";
    }
    return on_error( errorString );

}

//---------------The server thread-------------------------------------------
static void *startServer(void *arg) {
    UA_Server * server = arg;
    UA_LOG_INFO(UA_Log_Stdout, UA_LOGCATEGORY_USERLAND, "starting the server");

    UA_StatusCode retval = UA_Server_run(server, &running);

    UA_Server_delete(server);

    char *status = (char *)UA_StatusCode_name( retval );
    if (retval != UA_STATUSCODE_GOOD){
        UA_LOG_ERROR(UA_Log_Stdout, UA_LOGCATEGORY_USERLAND, "unable to start server, status %s", status);
    }

    // Set the flag to the ready state
    running = true;
    server = NULL;

    return status;
}

cJSON* opcua_server_add_nodes(cJSON* request) {
    cJSON *response = NULL;
    char *errorString = NULL;

    return on_ok(response);
error:
    cJSON_Delete( response );
    if (errorString == NULL){
        errorString = "programming error in opcua_server_start";
    }
    return on_error( errorString );
}

char *add_node(cJSON* node){
    char * result;

    cJSON *path = cJSON_GetObjectItemCaseSensitive(node, "path");
    cJSON *name = cJSON_GetObjectItemCaseSensitive(node, "name");
    cJSON *type = cJSON_GetObjectItemCaseSensitive(node, "type");
    cJSON *nodeId = cJSON_GetObjectItemCaseSensitive(node, "nodeId");


    /* Define the attribute of the myInteger variable node */
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
    
    return result;

// error:
//     return result;
}

char** str_split(char* a_str, const char a_delim){
    char** result    = 0;
    size_t count     = 0;
    char* tmp        = a_str;
    char* last_comma = 0;
    char delim[2];
    delim[0] = a_delim;
    delim[1] = 0;

    /* Count how many elements will be extracted. */
    while (*tmp)
    {
        if (a_delim == *tmp)
        {
            count++;
            last_comma = tmp;
        }
        tmp++;
    }

    /* Add space for trailing token. */
    count += last_comma < (a_str + strlen(a_str) - 1);

    /* Add space for terminating null string so caller
       knows where the list of returned strings ends. */
    count++;

    result = malloc(sizeof(char*) * count);

    if (result)
    {
        size_t idx  = 0;
        char* token = strtok(a_str, delim);

        while (token)
        {
            assert(idx < count);
            *(result + idx++) = strdup(token);
            token = strtok(0, delim);
        }
        assert(idx == count - 1);
        *(result + idx) = 0;
    }

    return result;
}

//------------------------catch signals------------------------------------------------
static void stopHandler(int sig) {
    UA_LOG_INFO(UA_Log_Stdout, UA_LOGCATEGORY_USERLAND, "received ctrl-c");
    running = false;
}

//------------------------THE ENTRY POINT------------------------------------------------
int main(int argc, char *argv[]) {

    signal(SIGINT, stopHandler);
    signal(SIGTERM, stopHandler);

    OpenSSL_add_all_algorithms();
    ERR_load_BIO_strings();

    // UA_LOG_INFO(UA_Log_Stdout, UA_LOGCATEGORY_USERLAND,"enter eport_loop");
    // eport_loop( &on_request );

    testStringSplit();
    
    
    return EXIT_SUCCESS;
}

void testStartServer(){
    UA_Server *server = UA_Server_new();
    UA_ServerConfig_setDefault(UA_Server_getConfig(server));
    startServer( server );
}

void testStringSplit(){
    char sample[] = "TAGS/my folder/my_tag";
    char** tokens;

    printf("sample=[%s]\n\n", sample);

    tokens = str_split(sample, '/');

    if (tokens)
    {
        int i;
        for (i = 0; *(tokens + i); i++)
        {
            printf("s=[%s]\n", *(tokens + i));
            free(*(tokens + i));
        }
        printf("\n");
        free(tokens);
    }

}