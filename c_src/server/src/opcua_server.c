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

//---------server thread entry point------
static void *startServer(void* arg);

// The RUN flag
static volatile UA_Boolean running = true;

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

    // Create a new server instance
    UA_Server *server = UA_Server_new();

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

    return status;
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

    UA_LOG_INFO(UA_Log_Stdout, UA_LOGCATEGORY_USERLAND,"enter eport_loop");
    eport_loop( &on_request );
    
    return EXIT_SUCCESS;
}