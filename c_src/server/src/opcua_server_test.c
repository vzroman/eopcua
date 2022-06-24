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
#include "utilities.h"
#include "opcua_server_test.h"

//----------local functions---------------
static void *server_thread(void *arg);

char *create_variable_test(char *path, char *type, cJSON *value, UA_NodeId *node);
char *ensure_path_test(char **path, int depth,  UA_NodeId folder, UA_NodeId *node);
char *find_in_folder_test( const UA_NodeId folder, const char *name, UA_NodeId *nodeId);
char *create_folder_test( const UA_NodeId folder, const char *name, UA_NodeId *nodeId);

//---------local flags-------------------
UA_Server *test_server = NULL;
static volatile UA_Boolean running = true;

int test_server_start(void){

    test_server = UA_Server_new();
    UA_ServerConfig *config = UA_Server_getConfig(test_server);

    // host and port
    UA_String host = UA_STRING_ALLOC("localhost");
    UA_Int16 port = 4840;


    // No encryption
    // UA_ServerConfig_setDefault(config);

    // encryption
    UA_ByteString certificate = loadFile("eopcua.cert.der");
    UA_ByteString privateKey = loadFile("eopcua.key.pem");
    if (certificate.length == 0){
        printf("\r\n------------------INVALID CERTIFICATE-------------------\r\n");
    }

    /* Loading of a issuer list, not used in this application */
    size_t trustListSize = 0;
    UA_ByteString *trustList = NULL;
    /* Loading of a issuer list, not used in this application */
    size_t issuerListSize = 0;
    UA_ByteString *issuerList = NULL;

    /* Loading of a revocation list currently unsupported */
    UA_ByteString *revocationList = NULL;
    size_t revocationListSize = 0;

    UA_ServerConfig_setDefaultWithSecurityPolicies(config, port,
                                                       &certificate, &privateKey,
                                                       trustList, trustListSize,
                                                       issuerList, issuerListSize,
                                                       revocationList, revocationListSize);
    config->customHostname = host;

    config->buildInfo.productName = UA_STRING_ALLOC("Faceplate OPCUA Server");
    config->buildInfo.productUri = UA_STRING_ALLOC("http://faceplate.io");
    config->buildInfo.manufacturerName = UA_STRING_ALLOC("Faceplate");
    config->buildInfo.softwareVersion = UA_STRING_ALLOC("0.0.1");

    // The same as buildInfo.productName 
    config->applicationDescription.applicationName.text = UA_STRING_ALLOC("Faceplate OPCUA Server");
    config->applicationDescription.applicationUri = UA_STRING_ALLOC("urn:faceplate.io:Faceplate:OPCUA:Server");
    // The same as buildInfo.productUri
    config->applicationDescription.productUri = UA_STRING_ALLOC("http://faceplate.io");
    

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
    if(retval != UA_STATUSCODE_GOOD) {
        UA_LOG_ERROR(UA_Log_Stdout, UA_LOGCATEGORY_SERVER, "Could not setup acess control StatusCode %s", UA_StatusCode_name(retval));
        return EXIT_FAILURE;
    }

    // start the server
    pthread_t serverThread;
    int res = pthread_create( &serverThread, NULL, &server_thread, NULL);
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

    printf("enter UA_Server_run");
    UA_StatusCode retval = UA_Server_run(test_server, &running);
    printf("server thread exit");

    UA_Server_delete(test_server);
    test_server = NULL;

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


int add_simple_node_test(char *path, char *type, cJSON *value) {

    UA_NodeId nodeId;
    create_variable_test(path, type, value, &nodeId);

    // UA_NodeId dirId; /* get the nodeid assigned by the server */
    // UA_ObjectAttributes dirAttr = UA_ObjectAttributes_default;
    // dirAttr.displayName = UA_LOCALIZEDTEXT("en-US", "RootDirectory");
    // UA_Server_addObjectNode(test_server, UA_NODEID_NULL,
    //                         UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER),
    //                         UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES),
    //                         UA_QUALIFIEDNAME(1, "RootDirectory"), UA_NODEID_NUMERIC(0, UA_NS0ID_BASEOBJECTTYPE),
    //                         dirAttr, NULL, &dirId);

    // UA_NodeId dir1Id; /* get the nodeid assigned by the server */
    // UA_ObjectAttributes dir1Attr = UA_ObjectAttributes_default;
    // dir1Attr.displayName = UA_LOCALIZEDTEXT("en-US", "SubDirectory");
    // UA_Server_addObjectNode(test_server, UA_NODEID_NULL,dirId,
    //                         UA_NODEID_NUMERIC(0, UA_NS0ID_HASCOMPONENT),
    //                         UA_QUALIFIEDNAME(1, "SubDirectory"), UA_NODEID_NUMERIC(0, UA_NS0ID_BASEOBJECTTYPE),
    //                         dir1Attr, NULL, &dir1Id);

    // UA_VariableAttributes varAttr = UA_VariableAttributes_default;
    // varAttr.accessLevel = UA_ACCESSLEVELMASK_READ | UA_ACCESSLEVELMASK_WRITE;
    // UA_Double rpm = 50.0;
    // UA_Variant_setScalar(&varAttr.value, &rpm, &UA_TYPES[UA_TYPES_DOUBLE]);
    // varAttr.displayName = UA_LOCALIZEDTEXT("en-US", "MyDoubleVariable");
    // UA_Server_addVariableNode(test_server, UA_NODEID_NULL, dir1Id,
    //                           UA_NODEID_NUMERIC(0, UA_NS0ID_HASCOMPONENT),
    //                           UA_QUALIFIEDNAME(1, "MyDoubleVariable"),
    //                           UA_NODEID_NUMERIC(0, UA_NS0ID_BASEDATAVARIABLETYPE), varAttr, NULL, NULL);

    // int pathSize = 3;
    // char *paths[3] = {"RootDirectory", "SubDirectory", "MyDoubleVariable"};
    // UA_QualifiedName *bPath = (UA_QualifiedName*)UA_Array_new(pathSize, &UA_TYPES[UA_TYPES_QUALIFIEDNAME]);
    // for(size_t i = 0; i < pathSize; i++) {
    //     UA_QualifiedName *elem = &bPath[i];
    //     elem->namespaceIndex = 1;
    //     elem->name = UA_STRING_ALLOC(paths[i]);
    // }

    // UA_BrowsePathResult bResult = UA_Server_browseSimplifiedBrowsePath(test_server,UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER),pathSize,bPath);

    // printf("\r\n---------------------------UA_Server_browseSimplifiedBrowsePath----------------------------\r\n");
    // printf("\tbResult.statusCode %s\r\n",(char*)UA_StatusCode_name( bResult.statusCode ));
    // printf("\tbResult.targetsSize %lu\r\n",bResult.targetsSize);

    // for(size_t j = 0; j < bResult.targetsSize; j++) {
    //     printf("\tbResult.targetsSize %lu\r\n",bResult.targets[j].targetId.nodeId);
    // }
    // UA_Server_addVariableNode(server, myIntegerNodeId, parentNodeId,
    //                           parentReferenceNodeId, myIntegerName,
    //                           UA_NODEID_NUMERIC(0, UA_NS0ID_BASEDATAVARIABLETYPE), attr, NULL, NULL);


    // UA_BrowsePathResult response = UA_Server_browseSimplifiedBrowsePath(server, const UA_NodeId origin,
    //                                  size_t browsePathSize,
    //                                  const UA_QualifiedName *browsePath);

    // #define BROWSE_PATHS_SIZE 3
    // char *paths[BROWSE_PATHS_SIZE] = {"Server", "ServerStatus", "State"};
    // UA_UInt32 ids[BROWSE_PATHS_SIZE] = {UA_NS0ID_ORGANIZES, UA_NS0ID_HASCOMPONENT, UA_NS0ID_HASCOMPONENT};
    // UA_BrowsePath browsePath;
    // UA_BrowsePath_init(&browsePath);
    // browsePath.startingNode = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
    // browsePath.relativePath.elements = (UA_RelativePathElement*)UA_Array_new(BROWSE_PATHS_SIZE, &UA_TYPES[UA_TYPES_RELATIVEPATHELEMENT]);
    // browsePath.relativePath.elementsSize = BROWSE_PATHS_SIZE;

    // for(size_t i = 0; i < BROWSE_PATHS_SIZE; i++) {
    //     UA_RelativePathElement *elem = &browsePath.relativePath.elements[i];
    //     elem->referenceTypeId = UA_NODEID_NUMERIC(0, ids[i]);
    //     elem->targetName = UA_QUALIFIEDNAME_ALLOC(0, paths[i]);
    // }
    return EXIT_SUCCESS;

}

char *create_variable_test(char *path, char *type, cJSON *value, UA_NodeId *node) {
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
        error = ensure_path_test(tokens, depth-1, folder, &folder);
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
    printf("\r\n---------------create variable %s---------------------\r\n",name);
    UA_VariableAttributes attr = UA_VariableAttributes_default;
    attr.accessLevel = UA_ACCESSLEVELMASK_READ | UA_ACCESSLEVELMASK_WRITE;
    attr.displayName = UA_LOCALIZEDTEXT_ALLOC("en-US", name);
    attr.dataType = ua_type->typeId;
    UA_QualifiedName qname = UA_QUALIFIEDNAME_ALLOC(1, name);

    free(name);
    name = NULL;

    // UA_Variant *ua_value = json2ua(ua_type, value);
    // if (ua_value == NULL){
    //     error = "invalid value";
    //     goto on_error;
    // }
    // sc = UA_Variant_copy(ua_value, &attr.value);
    // UA_Variant_delete(ua_value);
    // if (sc != UA_STATUSCODE_GOOD){
    //     error = (char*)UA_StatusCode_name( sc );
    //     goto on_error;
    // }

    sc = UA_Server_addVariableNode(test_server, 
        UA_NODEID_NULL, 
        folder, 
        referenceTypeId,
        qname,
        UA_NODEID_NUMERIC(0, UA_NS0ID_BASEDATAVARIABLETYPE), 
        attr, 
        NULL, 
        node);
    printf("\r\n\tcreate variable %sstatus: %s\r\n",name,(char*)UA_StatusCode_name( sc ));
    //UA_VariableAttributes_clear( &attr );
    if (sc != UA_STATUSCODE_GOOD){
        error = (char*)UA_StatusCode_name( sc );
        goto on_error;
    }

on_error:
    return error;
}


char *ensure_path_test(char **path, int depth,  UA_NodeId folder, UA_NodeId *node) {

    char *error = NULL;

    for (int i = 0; i < depth; i++){
        char *name = *(path + i);

        printf("\r\n---------find %s-----------------\r\n",name);
        error = find_in_folder_test( *node, name, node );
        printf("\r\n\t%s find_in_folder: %s\r\n",name,error);
        if (error != NULL) {
            if (strcmp(error,"not found") == 0){
                error = create_folder_test(*node, name, node);
                printf("\r\n%s create result %s\r\n",name,error);
                if (error != NULL) break;
            }else { 
                break; 
            }
        }
    }
    
    return error;
}

char *find_in_folder_test( const UA_NodeId folder, const char *name, UA_NodeId *nodeId ){
    
    char *error = NULL;
    UA_StatusCode sc;
    
    UA_QualifiedName qname = UA_QUALIFIEDNAME(1, (char *)name);
    UA_BrowsePathResult result = UA_Server_browseSimplifiedBrowsePath(test_server, folder, 1, &qname);

    printf("\r\n\t%s find_in_folder: %s",name, (char*)UA_StatusCode_name( result.statusCode ));
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

char *create_folder_test( const UA_NodeId folder, const char *name, UA_NodeId *nodeId ){
    char *error = NULL;
    UA_StatusCode sc;

    UA_NodeId referenceTypeId;
    UA_NodeId root = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);

    printf("\r\n------------------create %s-------------------\r\n",name);
    if (UA_NodeId_equal(&folder,&root)){
        referenceTypeId = UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES);
    }else{
        referenceTypeId = UA_NODEID_NUMERIC(0, UA_NS0ID_HASCOMPONENT);
    }

    UA_ObjectAttributes attr = UA_ObjectAttributes_default;
    attr.displayName = UA_LOCALIZEDTEXT("en-US", (char *)name);
    UA_QualifiedName qname = UA_QUALIFIEDNAME(1, (char *)name);

    sc = UA_Server_addObjectNode(
        test_server, 
        UA_NODEID_NULL,
        folder,
        referenceTypeId,
        qname, 
        UA_NODEID_NUMERIC(0, UA_NS0ID_BASEOBJECTTYPE),
        attr, 
        NULL, 
        nodeId);

    printf("\r\n\tcreate %s status: %s\r\n",name, (char*)UA_StatusCode_name( sc ));
    //UA_ObjectAttributes_clear( &attr );
    if (sc != UA_STATUSCODE_GOOD){
        error = (char*)UA_StatusCode_name( sc );
    }
    
    return error;

}