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
#include <pthread.h>
//----------------------------------------
#include <open62541/plugin/log_stdout.h>
#include <open62541/server.h>
#include <open62541/server_config_default.h>
#include <open62541/plugin/accesscontrol_default.h>

#include <open62541/client_config_default.h>
#include <open62541/client_highlevel.h>
#include <open62541/plugin/log_stdout.h>
//----------------------------------------
#include <openssl/x509v3.h>
#include <openssl/bn.h>
#include <openssl/asn1.h>
#include <openssl/x509.h>
#include <openssl/x509_vfy.h>
#include <openssl/pem.h>
#include <openssl/bio.h>

//----------------------------------------
#include "opcua_server_test.h"

//----------local functions---------------
static void *server_thread(void *arg);

//---------local flags-------------------
static volatile UA_Boolean running = true;

int test_server_start(void){

    UA_Server *server = UA_Server_new();
    UA_ServerConfig *config = UA_Server_getConfig(server);
    UA_ServerConfig_setDefault(config);

    config->buildInfo.productName = UA_STRING_ALLOC("Faceplate OPCUA Server");
    config->buildInfo.productUri = UA_STRING_ALLOC("http://faceplate.io");
    config->buildInfo.manufacturerName = UA_STRING_ALLOC("Faceplate");
    config->buildInfo.softwareVersion = UA_STRING_ALLOC("0.0.1");

    // The same as buildInfo.productName 
    config->applicationDescription.applicationName.text = UA_STRING_ALLOC("Faceplate OPCUA Server");
    config->applicationDescription.applicationUri = UA_STRING_ALLOC("urn:faceplate.io:Faceplate:OPCUA:Server");
    // The same as buildInfo.productUri
    config->applicationDescription.productUri = UA_STRING_ALLOC("http://faceplate.io");
    config->customHostname = UA_STRING_ALLOC("localhost");

    // Add users
    int userCount = 1;
    UA_String userLogin = UA_STRING_ALLOC("kostya");
    UA_String userPass = UA_STRING_ALLOC("111111");

    UA_UsernamePasswordLogin *users = (UA_UsernamePasswordLogin*)
        UA_malloc(userCount * sizeof(UA_UsernamePasswordLogin));

    if(!users) return UA_STATUSCODE_BADOUTOFMEMORY;
    for(size_t i = 0; i < userCount; i++) {
        UA_String_copy(&userLogin, &users[i].username);
        UA_String_copy(&userPass, &users[i].password);
    }
    config->accessControl.clear(&config->accessControl);
    UA_StatusCode retval = UA_AccessControl_default(config, false, NULL,
             &config->securityPolicies[config->securityPoliciesSize-1].policyUri, userCount, users);

    // start the server
    pthread_t serverThread;
    int res = pthread_create( &serverThread, NULL, &server_thread, server);
    if (res !=0 ){
        return EXIT_FAILURE;
    }
    printf("test server started\r\n");
    return EXIT_SUCCESS;
}

int test_server_stop(void){
    running = false;
    return EXIT_SUCCESS;
}

static void *server_thread(void *arg) {
    printf("server thread starting");
    UA_Server * server = arg;

    printf("enter UA_Server_run");
    UA_StatusCode retval = UA_Server_run(server, &running);
    printf("server thread exit");

    UA_Server_delete(server);

    char *status = (char *)UA_StatusCode_name( retval );

    return status;
}

int discovery_test(char *url) {


    /* Example for calling FindServers */
    UA_ApplicationDescription *applicationDescriptionArray = NULL;
    size_t applicationDescriptionArraySize = 0;

    UA_StatusCode retval;
    {
        UA_Client *client = UA_Client_new();
        UA_ClientConfig_setDefault(UA_Client_getConfig(client));
        retval = UA_Client_findServers(client, url, 0, NULL, 0, NULL,
                                       &applicationDescriptionArraySize, &applicationDescriptionArray);
        UA_Client_delete(client);
    }
    if(retval != UA_STATUSCODE_GOOD) {
        UA_LOG_ERROR(UA_Log_Stdout, UA_LOGCATEGORY_SERVER, "Could not call FindServers service. "
                "Is the discovery server started? StatusCode %s", UA_StatusCode_name(retval));
        return EXIT_FAILURE;
    }

    // output all the returned/registered servers
    for(size_t i = 0; i < applicationDescriptionArraySize; i++) {
        UA_ApplicationDescription *description = &applicationDescriptionArray[i];
        printf("Server[%lu]: %.*s", (unsigned long) i, (int) description->applicationUri.length,
               description->applicationUri.data);
        printf("\n\tName: %.*s", (int) description->applicationName.text.length,
               description->applicationName.text.data);
        printf("\n\tApplication URI: %.*s", (int) description->applicationUri.length,
               description->applicationUri.data);
        printf("\n\tProduct URI: %.*s", (int) description->productUri.length,
               description->productUri.data);
        printf("\n\tType: ");
        switch(description->applicationType) {
            case UA_APPLICATIONTYPE_SERVER:
                printf("Server");
                break;
            case UA_APPLICATIONTYPE_CLIENT:
                printf("Client");
                break;
            case UA_APPLICATIONTYPE_CLIENTANDSERVER:
                printf("Client and Server");
                break;
            case UA_APPLICATIONTYPE_DISCOVERYSERVER:
                printf("Discovery Server");
                break;
            default:
                printf("Unknown");
        }
        printf("\n\tDiscovery URLs:");
        for(size_t j = 0; j < description->discoveryUrlsSize; j++) {
            printf("\n\t\t[%lu]: %.*s", (unsigned long) j,
                   (int) description->discoveryUrls[j].length,
                   description->discoveryUrls[j].data);
        }
        printf("\n\n");
    }


    /*
     * Now that we have the list of available servers, call get endpoints on all of them
     */

    printf("-------- Server Endpoints --------\n");

    for(size_t i = 0; i < applicationDescriptionArraySize; i++) {
        UA_ApplicationDescription *description = &applicationDescriptionArray[i];
        if(description->discoveryUrlsSize == 0) {
            UA_LOG_INFO(UA_Log_Stdout, UA_LOGCATEGORY_CLIENT,
                        "[GetEndpoints] Server %.*s did not provide any discovery urls. Skipping.",
                        (int)description->applicationUri.length, description->applicationUri.data);
            continue;
        }

        printf("\nEndpoints for Server[%lu]: %.*s\n", (unsigned long) i,
               (int) description->applicationUri.length, description->applicationUri.data);

        UA_Client *client = UA_Client_new();
        UA_ClientConfig_setDefault(UA_Client_getConfig(client));

        char *discoveryUrl = (char *) UA_malloc(sizeof(char) * description->discoveryUrls[0].length + 1);
        memcpy(discoveryUrl, description->discoveryUrls[0].data, description->discoveryUrls[0].length);
        discoveryUrl[description->discoveryUrls[0].length] = '\0';

        UA_EndpointDescription *endpointArray = NULL;
        size_t endpointArraySize = 0;
        //TODO: adapt to the new async getEndpoint
        retval = UA_Client_getEndpoints(client, discoveryUrl, &endpointArraySize, &endpointArray);
        UA_free(discoveryUrl);
        if(retval != UA_STATUSCODE_GOOD) {
            UA_Client_disconnect(client);
            UA_Client_delete(client);
            break;
        }

        for(size_t j = 0; j < endpointArraySize; j++) {
            UA_EndpointDescription *endpoint = &endpointArray[j];
            printf("\n\tEndpoint[%lu]:", (unsigned long) j);
            printf("\n\t\tEndpoint URL: %.*s", (int) endpoint->endpointUrl.length, endpoint->endpointUrl.data);
            printf("\n\t\tTransport profile URI: %.*s", (int) endpoint->transportProfileUri.length,
                   endpoint->transportProfileUri.data);
            printf("\n\t\tSecurity Mode: ");
            switch(endpoint->securityMode) {
            case UA_MESSAGESECURITYMODE_INVALID:
                printf("Invalid");
                break;
            case UA_MESSAGESECURITYMODE_NONE:
                printf("None");
                break;
            case UA_MESSAGESECURITYMODE_SIGN:
                printf("Sign");
                break;
            case UA_MESSAGESECURITYMODE_SIGNANDENCRYPT:
                printf("Sign and Encrypt");
                break;
            default:
                printf("No valid security mode");
                break;
            }
            printf("\n\t\tSecurity profile URI: %.*s", (int) endpoint->securityPolicyUri.length,
                   endpoint->securityPolicyUri.data);
            printf("\n\t\tSecurity Level: %d", endpoint->securityLevel);
        }

        UA_Array_delete(endpointArray, endpointArraySize, &UA_TYPES[UA_TYPES_ENDPOINTDESCRIPTION]);
        UA_Client_delete(client);
    }

    printf("\n");

    UA_Array_delete(applicationDescriptionArray, applicationDescriptionArraySize,
                    &UA_TYPES[UA_TYPES_APPLICATIONDESCRIPTION]);

    return EXIT_SUCCESS;
}