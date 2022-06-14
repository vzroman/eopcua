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

//----------------------------------------
#include <eport_c.h>
//----------------------------------------
#include <open62541/client_config_default.h>
#include <open62541/client_highlevel.h>
#include <open62541/client_subscriptions.h>
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
#include "opcua_client.h"
   
cJSON* on_request(char* method, cJSON *args, char **error);

cJSON* opcua_client_connect(cJSON* args, char **error);
cJSON* opcua_client_read(cJSON* args, char **error);
cJSON* opcua_client_read_item(cJSON* args, char **error);
cJSON* opcua_client_write(cJSON* args, char **error);
cJSON* opcua_client_write_item(cJSON* args, char **error);
cJSON* opcua_client_subscribe(cJSON* args, char **error);
cJSON* opcua_client_subscribe_item(cJSON* args, char **error);
cJSON* opcua_client_update_subscriptions(cJSON* args, char **error);
cJSON* opcua_client_browse_endpoints(cJSON* args, char **error);
cJSON* opcua_client_browse_folder(cJSON* args, char **error);

int path2nodeId( cJSON *path, UA_NodeId *node );
char* path2string( cJSON *path );
cJSON* browse_folder( UA_NodeId folder, char **error );
cJSON* parse_value( UA_Variant *value );
int export_value(UA_Variant *ua_value, cJSON *value);
int init_subscriptions(void);
static void on_subscription_update(UA_Client *client, UA_UInt32 subId, void *subContext,
                         UA_UInt32 monId, void *monContext, UA_DataValue *value);
UA_StatusCode get_connection_state( UA_Client *client );

UA_ByteString* loadFile(const char* path);
UA_ByteString* parse_base64( char* base64string );
char* parse_certificate_uri( const unsigned char *certificate, size_t len );

// Global variables
UA_Client *opcua_client;
UA_UInt32 subscriptionId;

// Subscription indexes
opcua_client_binding *opcua_client_bindings = NULL;
opcua_client_subscription *opcua_client_subscriptions = NULL;

cJSON* on_request( char *method, cJSON *args, char **error ){
    
    cJSON *response = NULL;
    // Handle the request
    LOGDEBUG("handle the request");
    if( strcmp(method, "connect") == 0){
        response = opcua_client_connect( args, error );
    } else if (strcmp(method, "read") == 0){
        response = opcua_client_read( args, error );
    }else if (strcmp(method, "write") == 0){
        response = opcua_client_write( args, error );
    }else if (strcmp(method, "subscribe") == 0){
        response = opcua_client_subscribe( args, error );
    }else if (strcmp(method, "update_subscriptions") == 0){
        response = opcua_client_update_subscriptions( args, error );
    }else if (strcmp(method, "browse_endpoints") == 0){
        response = opcua_client_browse_endpoints( args, error );
    }else if (strcmp(method, "browse_folder") == 0){
        response = opcua_client_browse_folder( args, error );
    } else{
        *error = "invalid method";
    }

    return response;
}

// The expected structure is:
//     {
//         "host": "localhost",
//         "port": 4841,
//         ----optional---------
//         "endpoint": "OPCUA/SimulationServer",
//         "certificate": "<base64 encoded der>",
//         "privateKey": "<base64 encoded pem>",
//         "login":"user1",
//         "password":"secret"
//     }
cJSON* opcua_client_connect(cJSON* args, char **error){
    cJSON *response = NULL;

    cJSON *host = NULL;
    cJSON *endpoint = NULL;
    cJSON *port = NULL;
    cJSON *login = NULL;
    cJSON *certificate = NULL;
    cJSON *privateKey = NULL;
    cJSON *password = NULL;
    char *connectionString = NULL;
    char *URI = NULL;

    //---------------parsing arguments------------------------
    host = cJSON_GetObjectItemCaseSensitive(args, "host");
    if (!cJSON_IsString(host) || (host->valuestring == NULL)){
        *error = "host is not defined";
        goto on_error; 
    }
    port = cJSON_GetObjectItemCaseSensitive(args, "port");
    if (!cJSON_IsNumber(port)){
        *error = "port is not defined";
        goto on_error; 
    }

    endpoint = cJSON_GetObjectItemCaseSensitive(args, "endpoint");
    if (!cJSON_IsString(endpoint) || (endpoint->valuestring == NULL)){
        endpoint = NULL;
    }

    certificate = cJSON_GetObjectItemCaseSensitive(args, "certificate");
    if (cJSON_IsString(certificate) && (certificate->valuestring != NULL)){
        // It is a secure connection, the key must be provided
        privateKey = cJSON_GetObjectItemCaseSensitive(args, "private_key");
        if (!cJSON_IsString(privateKey) || (privateKey->valuestring == NULL)){
            *error = "key is not defined";
            goto on_error; 
        }
    }else{
        certificate = NULL;
    }

    login = cJSON_GetObjectItemCaseSensitive(args, "login");
    if (cJSON_IsString(login) && (login->valuestring != NULL)){

        // If the login is provided then the password is required
        password = cJSON_GetObjectItemCaseSensitive(args, "password");
        if (!cJSON_IsString(password) || (password->valuestring == NULL)){
            *error = "password is not defined";
            goto on_error; 
        }
    }else{
        login = NULL;
    }

    // Build the connection string (6 in tail is :<port> as port max string length is 5)
    char *prefix = "opc.tcp://";
    int urlLen = strlen(prefix) + strlen(host->valuestring) + 6; // :65535 is max
    if (endpoint != NULL){
        urlLen += strlen( endpoint->valuestring );
    }

    connectionString = malloc( urlLen );
    if (endpoint != NULL){
        sprintf(connectionString, "%s%s:%d/%s", prefix, host->valuestring, (int)port->valuedouble, endpoint->valuestring);
    }else{
        sprintf(connectionString, "%s%s:%d", prefix, host->valuestring, (int)port->valuedouble);
    }
    LOGINFO("connecting to %s",connectionString);

    //--------------Connecting procedure------------------------------
    UA_StatusCode retval;

    // get the config object
    UA_ClientConfig *config = UA_Client_getConfig(opcua_client);
    
    UA_ByteString *cert = NULL;
    UA_ByteString *key = NULL;

    // Configure the connection
    if (cJSON_IsString(certificate)){
        LOGTRACE("prepare secure connection");

        cert = parse_base64( certificate->valuestring );
        if (cert == NULL){
            *error = "unable to parse the certificate from base64";
            goto on_error;
        }

        key = parse_base64( privateKey->valuestring );
        if (key == NULL){
            *error = "unable to parse the key from base64";
            goto on_error;
        }

        // Parse the application URI from the certificate
        URI = parse_certificate_uri( (const unsigned char *)cert->data, cert->length );   
        if (URI == NULL){
            *error = "unable to parse the certificate";
            goto on_error;
        } 

        // Trust list
        size_t trustListSize = 0;
        UA_STACKARRAY(UA_ByteString, trustList, trustListSize);

        // Revocation list
        UA_ByteString *revocationList = NULL;
        size_t revocationListSize = 0;

        config->securityMode = UA_MESSAGESECURITYMODE_SIGNANDENCRYPT;
        if (UA_ClientConfig_setDefaultEncryption(config, *cert, *key,
                                            trustList, trustListSize,
                                            revocationList, revocationListSize) 
            != UA_STATUSCODE_GOOD){
            *error = "unable to configure a secure connection";
            goto on_error;
        };

        config->clientDescription.applicationUri = UA_STRING_ALLOC(URI);

        free(URI);
        URI = NULL;
    }else{
        UA_ClientConfig_setDefault(config);
    }

    if (cJSON_IsString(login)){
        // Authorized access
        LOGINFO("authorized connection to %s, user %s", connectionString,login->valuestring);
        retval = UA_Client_connectUsername(opcua_client, connectionString, login->valuestring, password->valuestring);
    }else{
        LOGINFO("anonymous connection to %s", connectionString);
        retval = UA_Client_connect(opcua_client, connectionString);
    }

    if(retval != UA_STATUSCODE_GOOD) {
        *error = "connection error";
        goto on_error;
    }

    if (init_subscriptions() != 0){
        *error = "init subscriptions error";
        goto on_error;
    }

    return cJSON_CreateString("ok");

on_error:
    if (URI != NULL){
        free(URI);
    }
    if (cert!= NULL){
        UA_ByteString_clear( cert );
    }
    if (key!= NULL){
        UA_ByteString_clear( key );
    }
    cJSON_Delete( response );
    return NULL;
}

//---------------------------------------------------------------
//  READ
//---------------------------------------------------------------
cJSON* opcua_client_read(cJSON* args, char **error){
    LOGTRACE("reading");
    cJSON *response = cJSON_CreateArray();
    cJSON *item = NULL;
    cJSON *value = NULL;

    if ( !cJSON_IsArray(args) ) {
        *error = "invalid read arguments";
        goto on_error;
    }

    cJSON_ArrayForEach(item, args) {
        value = opcua_client_read_item( item, error );
        if (value == NULL){
            value = cJSON_CreateString("error: read error");
        }
        if ( !cJSON_AddItemToArray(response, value) ){
            *error = "unable add a value for item";
            goto on_error;
        }
    }

    return response;
on_error:
    cJSON_Delete( response );
    return NULL;
}

cJSON* opcua_client_read_item(cJSON* args, char **error){
    cJSON *response = NULL;
    UA_Variant *value = UA_Variant_new();

    UA_NodeId nodeId = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
    int ret = path2nodeId( args, &nodeId );
    if (ret != 0){
        *error = "invalid node path";
        goto on_error;
    }

    /* Read the value */
    if (UA_Client_readValueAttribute(opcua_client, nodeId, value) != UA_STATUSCODE_GOOD ) {
        *error = "unable to read value";
        goto on_error;
    }

    response = parse_value( value ); 
    UA_Variant_delete(value);

    if (response == NULL){
        *error = "invalid value";
        goto on_error;
    }

    return response;

on_error:
    cJSON_Delete( response );
    return NULL;
}

//---------------------------------------------------------------
//  WRITE
//---------------------------------------------------------------
cJSON* opcua_client_write(cJSON* args, char **error){
    LOGTRACE("writing");
    cJSON *response = cJSON_CreateArray();
    cJSON *item = NULL;
    cJSON *result = NULL;

if ( !cJSON_IsArray(args) ) {
        *error = "invalid write parameters";
        goto on_error;
    }

    cJSON_ArrayForEach(item, args) {
        result = opcua_client_write_item( item, error );
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

cJSON* opcua_client_write_item(cJSON* args, char **error){
    cJSON *response = NULL;
    UA_Variant *ua_value = UA_Variant_new();

    cJSON *tag = cJSON_GetArrayItem(args, 0);
    cJSON *value = cJSON_GetArrayItem(args, 1);

    // Get the type of the tag
    UA_NodeId nodeId = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
    int ret = path2nodeId( tag, &nodeId );
    if (ret != 0){
        *error = "invalid node path";
        goto on_error;
    }
    if (UA_Client_readValueAttribute(opcua_client, nodeId, ua_value) != UA_STATUSCODE_GOOD ) {
        *error = "unable to define tag type";
        goto on_error;
    }

    if ( export_value(ua_value, value) != 0 ){
        *error = "invalid value";
        goto on_error;
    }

    // Write the value
    UA_StatusCode status = UA_Client_writeValueAttribute(opcua_client, nodeId, ua_value);
    if ( status != UA_STATUSCODE_GOOD ){
        *error = (char*)UA_StatusCode_name( status );
        goto on_error;
    }

    UA_Variant_delete(ua_value);

    return cJSON_CreateString("ok");

on_error:
    cJSON_Delete( response );
    UA_Variant_delete( ua_value );
    return NULL;
}

//---------------------------------------------------------------
//  SUBSCRIBE
//---------------------------------------------------------------
cJSON* opcua_client_subscribe(cJSON* args, char **error){
    LOGTRACE("subscribing");
    cJSON *response = cJSON_CreateArray();
    cJSON *item = NULL;
    cJSON *result = NULL;

    if ( !cJSON_IsArray(args) ) {
        *error = "invalid subscribe parameters";
        goto on_error;
    }

    cJSON_ArrayForEach(item, args) {
        result = opcua_client_subscribe_item( item, error );
        if (result == NULL){
            result = cJSON_CreateString("error: subscribe error");
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

cJSON* opcua_client_subscribe_item(cJSON* args, char **error){
    LOGTRACE("subscribing to an item");

    cJSON *response = NULL;
    char *path = NULL;
    

    // Get the key to the binding
    path = path2string( args );
    if (path == NULL){
        *error = "unable to convert the node path to string";
        goto on_error;
    }

    // Lookup the binding in the collection
    opcua_client_binding *b = NULL;
    opcua_client_subscription *s = NULL;
    HASH_FIND_STR(opcua_client_bindings, path, b);

    if (b == NULL){
        // The binding is not in the collection yet.
        // Create a new subscription.
        LOGTRACE("create a new subscription");

        // Get nodeId
        UA_NodeId nodeId = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
        int ret = path2nodeId( args, &nodeId );
        if (ret != 0){
            *error = "invalid node path";
            goto on_error;
        }

        // Add the binding to the monitored items
        UA_MonitoredItemCreateRequest monRequest = UA_MonitoredItemCreateRequest_default(nodeId);
        UA_MonitoredItemCreateResult monResponse =
        UA_Client_MonitoredItems_createDataChange(opcua_client, subscriptionId, UA_TIMESTAMPSTORETURN_BOTH,
                                                monRequest, NULL, on_subscription_update, NULL);

        LOGDEBUG("monResponse.monitoredItemId %d", monResponse.monitoredItemId);

        if(monResponse.statusCode != UA_STATUSCODE_GOOD){
            *error = "uanble to add a monitored item";
            goto on_error;
        }

        // Add the binding to the collection
        b = (opcua_client_binding *)malloc(sizeof *b);
        if (b == NULL){
            *error = "unable to allocate the memory for a new binding index";
            goto on_error;
        }
        b->path = path;
        b->id = monResponse.monitoredItemId;

        s = (opcua_client_subscription *)malloc(sizeof *s);
        if (s == NULL){
            *error = "unable to allocate the memory for new binding";
            goto on_error;
        }
        s->id = monResponse.monitoredItemId;
        s->value = UA_Variant_new();
        
        HASH_ADD_STR(opcua_client_bindings, path, b);
        HASH_ADD_INT(opcua_client_subscriptions, id, s);

        // The subscription is already created, we should not clear the path from this moment
        path = NULL;

        return opcua_client_read_item( args, error );
    }else{
        free( path );
        path = NULL;
        // The binding is already in the active subscriptions
        LOGTRACE("lookup the value in the active subscriptions");

        // Lookup the value
        HASH_FIND_INT(opcua_client_subscriptions, &b->id, s);
        if (s == NULL){
            *error = "invalid subscription index";
            goto on_error;
        }

        response = parse_value( s->value );
        if (response == NULL){
            *error = "invalid value";
            goto on_error;
        }
    }

    return response;

on_error:
    if (path != NULL){
        free(path);
    }
    cJSON_Delete( response );
    return NULL;
}

cJSON* opcua_client_update_subscriptions(cJSON* args, char **error){
    if (get_connection_state( opcua_client ) != UA_STATUSCODE_GOOD){
        *error = "connection error";
        goto on_error;
    };

    if (UA_Client_run_iterate(opcua_client, 10) != UA_STATUSCODE_GOOD){
        *error = "unable update subscriptions";
        goto on_error;
    }

    return cJSON_CreateString("ok");
on_error:
    return NULL;
}

UA_StatusCode get_connection_state( UA_Client *client ){
    
    const UA_NodeId nodeId = UA_NODEID_NUMERIC(0, UA_NS0ID_SERVER_SERVERSTATUS);

    UA_NodeId dataType;
    UA_NodeId_init(&dataType);

    return UA_Client_readDataTypeAttribute(client, nodeId, &dataType);
}

cJSON* opcua_client_browse_endpoints(cJSON* args, char **error){
    UA_Client *client = NULL;
    cJSON *response = NULL;
    cJSON *host = NULL;
    cJSON *port = NULL;
    cJSON *endpoint = NULL;
    char *connectionString = NULL;

    if ( !cJSON_IsObject(args) ) {
        *error = "invalid parameters";
        goto on_error;
    }

    host = cJSON_GetObjectItemCaseSensitive(args, "host");
    if (!cJSON_IsString(host) || (host->valuestring == NULL)){
        *error = "host is not defined";
        goto on_error; 
    }

    port = cJSON_GetObjectItemCaseSensitive(args, "port");
    if (!cJSON_IsNumber(port)){
        *error = "port is not defined";
        goto on_error; 
    }

    UA_EndpointDescription* endpointArray = NULL;
    size_t endpointArraySize = 0;
    UA_StatusCode retval;

    // Build the connection string (6 in tail is :<port> as port max string length is 5)
    char *prefix = "opc.tcp://";
    int urlLen = strlen(prefix) + strlen(host->valuestring) + 6; // :65535 is max
    connectionString = malloc( urlLen );
    if (connectionString == NULL){
        *error = "unable to allocate connectionString";
        goto on_error;
    }
    sprintf(connectionString, "%s%s:%d", prefix, host->valuestring, (int)port->valuedouble);
    LOGDEBUG("connectionString %s",connectionString);

    // Create a connection
    client = UA_Client_new();
    if (client == NULL){
        *error = "unable to allocate the client";
        goto on_error;
    }
    UA_ClientConfig_setDefault(UA_Client_getConfig(client));

    // Request endpoints
    retval = UA_Client_getEndpoints(client, connectionString, &endpointArraySize, &endpointArray);
    if(retval != UA_STATUSCODE_GOOD) {
        *error = "connection error";
        goto on_error;
    }
    UA_Client_delete(client); client = NULL;
    LOGDEBUG("%i endpoints found\r\n",(int)endpointArraySize);

    // Build the response
    response = cJSON_CreateArray();
    if(response == NULL){
        *error = "unable to allocate CJSON object for response";
        goto on_error;
    }
    for(size_t i=0; i<endpointArraySize; i++) {
        endpoint = cJSON_CreateString( (char *)endpointArray[i].server.discoveryUrls->data );
        if (endpoint == NULL){
            *error = "unable to allocate CJSON object for endpoint";
            goto on_error;
        }
        if (!cJSON_AddItemToArray(response,endpoint)){
            *error = "unable to add endpoint to the array";
            goto on_error;
        }
    }

    free(connectionString);
    UA_Array_delete(endpointArray,endpointArraySize, &UA_TYPES[UA_TYPES_ENDPOINTDESCRIPTION]);

    return response;

on_error:
    if (connectionString != NULL){
        free(connectionString);
    }
    if (endpointArray != NULL){
        UA_Array_delete(endpointArray, endpointArraySize, &UA_TYPES[UA_TYPES_ENDPOINTDESCRIPTION]);
    }
    if(client != NULL){
        UA_Client_delete(client);
    }
    cJSON_Delete( endpoint );
    cJSON_Delete( response );
    return NULL;
}

cJSON* opcua_client_browse_folder(cJSON* args, char **error){
    cJSON *response = NULL;

    if ( !cJSON_IsArray(args) ) {
        *error = "invalid folder parameter";
        goto on_error;
    }

    UA_NodeId nodeId = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
    int ret = path2nodeId( args, &nodeId );
    if (ret != 0){
        *error = "invalid node path";
        goto on_error;
    }

    return browse_folder( nodeId, error );

on_error:
    cJSON_Delete( response );
    return NULL;
}

cJSON* browse_folder( UA_NodeId folder, char **error ){
    cJSON *result = NULL;
    cJSON *subitem = NULL;
    UA_String nodeId;

    // Build the request
    UA_BrowseRequest request;
    UA_BrowseRequest_init(&request);
    request.requestedMaxReferencesPerNode = 0;
    request.nodesToBrowse = UA_BrowseDescription_new();
    request.nodesToBrowseSize = 1;
    request.nodesToBrowse[0].nodeId = folder; 
    request.nodesToBrowse[0].resultMask = UA_BROWSERESULTMASK_ALL;

    // Execute the request
    UA_BrowseResponse response = UA_Client_Service_browse(opcua_client, request);
    if (response.responseHeader.serviceResult != UA_STATUSCODE_GOOD){
        *error = "UA_Client_Service_browse error";
        goto on_error;
    }

    // Build the result
    result = cJSON_CreateObject();
    for(size_t i = 0; i < response.resultsSize; ++i) {
        for(size_t j = 0; j < response.results[i].referencesSize; ++j) {

            UA_ReferenceDescription *ref = &(response.results[i].references[j]);

            // Convert the nodeId to string
            if (UA_NodeId_print(&ref->nodeId.nodeId, &nodeId) != UA_STATUSCODE_GOOD){
                *error = "unable to serialize nodeId in browse_folder";
                goto on_error; 
            }

            subitem = cJSON_CreateObject();
            if(subitem == NULL){
                *error = "unable to allocate subitem";
                goto on_error;
            }
            
            if (cJSON_AddStringToObject(subitem,"id", (char *)nodeId.data) == NULL) {
                *error = "unable to add a nodeId the result in browse_folder";
                UA_String_clear(&nodeId);
                goto on_error; 
            }

            if (cJSON_AddNumberToObject(subitem,"type", ref->nodeClass) == NULL) {
                *error = "unable to add a type the result in browse_folder";
                UA_String_clear(&nodeId);
                goto on_error; 
            }

            if (!cJSON_AddItemToObject(result, (char *)ref->displayName.text.data, subitem)) {
                *error = "unable to add a node the result in browse_folder";
                UA_String_clear(&nodeId);
                goto on_error; 
            }

            UA_String_clear(&nodeId);
        }
    }
    UA_BrowseRequest_clear(&request);
    UA_BrowseResponse_clear(&response);

    return result;

on_error:
    cJSON_Delete( subitem );
    cJSON_Delete( result );
    UA_BrowseRequest_clear(&request);
    UA_BrowseResponse_clear(&response);
    return NULL;
}

//---------------------------------------------------------------------------
//  Internal helpers
//---------------------------------------------------------------------------
int path2nodeId( cJSON *path, UA_NodeId *node ){
    cJSON *level = NULL;
    cJSON *content = NULL;
    cJSON *next = NULL;
    char **error = NULL;

    // Start from the Objects folder
    cJSON_ArrayForEach(level, path) {
        content = browse_folder( *node, error );
        if (content == NULL){
            LOGERROR("path2nodeId unable to browse folder");
            goto on_error;
        }

        // Lookup node by name
        next = cJSON_GetObjectItemCaseSensitive(content, level->valuestring );
        if (!cJSON_IsObject(next)){
            LOGERROR("path2nodeId invalid level %s",level->valuestring);
            goto on_error;
        }
        next = cJSON_GetObjectItemCaseSensitive(next, "id" );
        if (!cJSON_IsString(next) || (next->valuestring == NULL)){
            LOGERROR("path2nodeId invalid level %s",level->valuestring);
            goto on_error;
        }

        if (UA_NodeId_parse(node, UA_STRING((char*)(uintptr_t)next->valuestring)) != UA_STATUSCODE_GOOD){
            LOGERROR("path2nodeId unable to parse nodeId %s",next->valuestring);
            goto on_error;
        }
        cJSON_Delete( content );
    }

    return 0;

on_error:
    cJSON_Delete( content );
    return -1;
}

char* path2string( cJSON *path ){
    cJSON *level = NULL;
    int length = 0;
    char *result = NULL;

    // Get the required size 
    cJSON_ArrayForEach(level, path) {
        length += 1 + strlen( level->valuestring );
    }

    // Allocate the string
    result = malloc( length );
    if (result == NULL){
        LOGERROR("unable to allocate the memory for path string");
        goto error;
    }

    // Join
    strcpy(result, "");
    level = NULL;
    cJSON_ArrayForEach(level, path) {
        strcat(result, "/");
        strcat(result, level->valuestring);
    }

    LOGDEBUG("path2string %s", result);

    return result;

error:
    if (result != NULL){
        free(result);
    }
    return NULL;
}

cJSON* parse_value( UA_Variant *ua_value ){
    cJSON *value = NULL;
    if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_BOOLEAN]) ){
        value = cJSON_CreateBool( *(UA_Boolean*)ua_value->data );
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_SBYTE]) ){
        value = cJSON_CreateNumber( *(UA_SByte*)ua_value->data );
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_BYTE]) ){
        value = cJSON_CreateNumber( *(UA_Byte*)ua_value->data );
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_INT16]) ){
        value = cJSON_CreateNumber( *(UA_Int16*)ua_value->data );
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_UINT16]) ){
        value = cJSON_CreateNumber( *(UA_UInt16*)ua_value->data );
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_INT32]) ){
        value = cJSON_CreateNumber( *(UA_Int32*)ua_value->data );
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_UINT32]) ){
        value = cJSON_CreateNumber( *(UA_UInt32*)ua_value->data );
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_INT64]) ){
        value = cJSON_CreateNumber( *(UA_Int64*)ua_value->data );
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_UINT64]) ){
        value = cJSON_CreateNumber( *(UA_UInt64*)ua_value->data );
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_FLOAT]) ){
        value = cJSON_CreateNumber( *(UA_Float*)ua_value->data );
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_DOUBLE]) ){
        value = cJSON_CreateNumber( *(UA_Double*)ua_value->data );
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_STRING]) ){
        value = cJSON_CreateString( (char *)((UA_String*)ua_value->data)->data );
    }
    // TODO. Support other types
    return value;
}

int export_value(UA_Variant *ua_value, cJSON *value){
    int retval = -1;
    if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_BOOLEAN]) ){
        if ( cJSON_IsBool(value) || cJSON_IsNumber(value) ) {
            UA_Boolean v = (value->valuedouble != 0) ;
            if ( UA_Variant_setScalarCopy( ua_value, &v, &UA_TYPES[UA_TYPES_BOOLEAN]) == UA_STATUSCODE_GOOD) {
                retval = 0;
            }
        }
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_SBYTE]) ){
        if ( cJSON_IsBool(value) || cJSON_IsNumber(value) ) {
            UA_SByte v = (UA_SByte)value->valuedouble;
            if ( UA_Variant_setScalarCopy( ua_value, &v, &UA_TYPES[UA_TYPES_SBYTE]) == UA_STATUSCODE_GOOD){
                retval = 0;
            }
        }
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_BYTE]) ){
        if ( cJSON_IsBool(value) || cJSON_IsNumber(value) ) {
            UA_Byte v = (UA_Byte)value->valuedouble;
            if (UA_Variant_setScalarCopy( ua_value, &v, &UA_TYPES[UA_TYPES_BYTE]) == UA_STATUSCODE_GOOD){
                retval = 0;
            }
        }
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_INT16]) ){
        if ( cJSON_IsBool(value) || cJSON_IsNumber(value) ) {
            UA_Int16 v = (UA_Int16)value->valuedouble;
            if (UA_Variant_setScalarCopy( ua_value, &v, &UA_TYPES[UA_TYPES_INT16]) == UA_STATUSCODE_GOOD){
                retval = 0;
            }
        }
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_UINT16]) ){
        if ( cJSON_IsBool(value) || cJSON_IsNumber(value) ) {
            UA_UInt16 v = (UA_UInt16)value->valuedouble;
            if (UA_Variant_setScalarCopy( ua_value, &v, &UA_TYPES[UA_TYPES_UINT16]) == UA_STATUSCODE_GOOD){
                retval = 0;
            }
        }
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_INT32]) ){
        if ( cJSON_IsBool(value) || cJSON_IsNumber(value) ) {
            UA_Int32 v = (UA_Int32)value->valuedouble;
            if (UA_Variant_setScalarCopy( ua_value, &v, &UA_TYPES[UA_TYPES_INT32]) == UA_STATUSCODE_GOOD){
                retval = 0;
            }
        }
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_UINT32]) ){
        if ( cJSON_IsBool(value) || cJSON_IsNumber(value) ) {
            UA_UInt32 v = (UA_UInt32)value->valuedouble;
            if ( UA_Variant_setScalarCopy( ua_value, &v, &UA_TYPES[UA_TYPES_UINT32]) == UA_STATUSCODE_GOOD){
                retval = 0;
            }
        }
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_INT64]) ){
        if ( cJSON_IsBool(value) || cJSON_IsNumber(value) ) {
            UA_Int64 v = (UA_Int64)value->valuedouble;
            if (UA_Variant_setScalarCopy( ua_value, &v, &UA_TYPES[UA_TYPES_INT64]) == UA_STATUSCODE_GOOD){
                retval = 0;
            }
        }
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_UINT64]) ){
        if ( cJSON_IsBool(value) || cJSON_IsNumber(value) ) {
            UA_UInt64 v = (UA_UInt64)value->valuedouble;
            if (UA_Variant_setScalarCopy( ua_value, &v, &UA_TYPES[UA_TYPES_UINT64]) == UA_STATUSCODE_GOOD){
                retval = 0;
            }
        }
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_FLOAT]) ){
        if ( cJSON_IsBool(value) || cJSON_IsNumber(value) ) {
            UA_Float v = (UA_Float)value->valuedouble;
            if (UA_Variant_setScalarCopy( ua_value, &v, &UA_TYPES[UA_TYPES_FLOAT]) == UA_STATUSCODE_GOOD){
                retval = 0;
            }
        }
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_DOUBLE]) ){
        if ( cJSON_IsBool(value) || cJSON_IsNumber(value) ) {
            UA_Double v = (UA_Double)value->valuedouble;
            if (UA_Variant_setScalarCopy( ua_value, &v, &UA_TYPES[UA_TYPES_DOUBLE]) == UA_STATUSCODE_GOOD){
                retval = 0;
            }
        }
    }else if( UA_Variant_hasScalarType( ua_value, &UA_TYPES[UA_TYPES_STRING]) ){
        if ( cJSON_IsString(value) && value->valuestring != NULL ) {
            UA_String  v =  UA_STRING((char *)value->valuestring);
            if (UA_Variant_setScalarCopy( ua_value, &v, &UA_TYPES[UA_TYPES_STRING]) == UA_STATUSCODE_GOOD){
                retval = 0;
            }
        }
    }
    return retval;
}

int init_subscriptions(){

    UA_CreateSubscriptionRequest request = UA_CreateSubscriptionRequest_default();
    UA_CreateSubscriptionResponse response = UA_Client_Subscriptions_create(opcua_client, request, NULL, NULL, NULL);
 
    subscriptionId = response.subscriptionId;
    if(response.responseHeader.serviceResult != UA_STATUSCODE_GOOD){
        LOGERROR("unable to create a subscription request");
        goto error;
    }
    LOGDEBUG("Create subscription succeeded, id %u", subscriptionId);
 
   return 0;
error:
    if(UA_Client_Subscriptions_deleteSingle(opcua_client, subscriptionId) == UA_STATUSCODE_GOOD){
        LOGERROR("unable to purge the subscription request");
    }
    return -1;
}

static void on_subscription_update(UA_Client *client, UA_UInt32 subId, void *subContext,
                         UA_UInt32 monId, void *monContext, UA_DataValue *value) {

    // Lookup the subscription
    opcua_client_subscription *s = NULL;
    HASH_FIND_INT(opcua_client_subscriptions, &monId, s);
    if (s != NULL){
        // Update the value
        LOGTRACE("update subscription %d",monId);
        if (UA_Variant_copy( &value->value, s->value ) != UA_STATUSCODE_GOOD){
            LOGERROR("unable to copy value on subscription update");
        }
    }
}

UA_ByteString* loadFile(const char* path){
	FILE* f = fopen(path, "rb");
	if (f == NULL){
        return NULL;
    }
	fseek(f, 0, SEEK_END);
	long fsize = ftell(f);
	fseek(f, 0, SEEK_SET); 

	UA_ByteString* result = UA_ByteString_new();
	UA_ByteString_allocBuffer(result, (size_t)fsize + 1);
	memset(result->data, 0, result->length);
	if (fread(result->data, result->length, 1, f) == 0){
	    LOGDEBUG("empty file");
	}
	fclose(f);

	return result;
}

UA_ByteString* parse_base64(char* base64string){
    UA_ByteString *result = UA_ByteString_new();

    LOGDEBUG("allocate UA_String for base64");
    UA_String b64 = UA_STRING_ALLOC( base64string );

    LOGDEBUG("parse base64 from UA_String");
    if ( UA_ByteString_fromBase64( result, &b64 ) != UA_STATUSCODE_GOOD){
        LOGERROR("unable to parse base64 string");
        goto error;
    };
    UA_String_clear( &b64 );

	return result;

error:
    UA_ByteString_clear( result );
    UA_String_clear( &b64 );
    return NULL;
}

char* parse_certificate_uri( const unsigned char *certificate, size_t len ){
    X509 *cert = NULL;
    X509_EXTENSION *ex = NULL;
    BIO *ext_bio = NULL;
    BUF_MEM *bptr = NULL;
    char *URI = NULL;

    // Parse the certificate
    cert = d2i_X509(NULL, &certificate, len);
    if (!cert) {
        LOGERROR("unable to parse certificate in memory");
        goto error;
    }

    // Extract the subjectAltName extension
    int index = X509_get_ext_by_NID( cert, NID_subject_alt_name, -1);
    if (index < 0 ){
        LOGERROR("the certificate doesn't have the subjectAltName extension");
        goto error;
    }
    ex = X509_get_ext(cert, index);
    if (ex == NULL){
        LOGERROR("unable to extract subjectAltName extension");
        goto error;
    }

    // get the extension value
    ext_bio = BIO_new(BIO_s_mem());
    if (ext_bio == NULL){
        LOGERROR("unable to allocate memory for extension value BIO");
        goto error;
    }
    if(!X509V3_EXT_print(ext_bio, ex, 0, 0)){
        LOGERROR("unable to allocate memory for extension value BIO");
        goto error;
    }
    BIO_flush(ext_bio);
    BIO_get_mem_ptr(ext_bio, &bptr);

    // Find the URI in the value
    // example - URI:urn:faceplate.io:Faceplate:opcuadriver, DNS:localhost
    int URIStart = -1;
    int URIStop = -1;
    for (int i=0; i<bptr->length; i++){
        if ((URIStart == -1) && ( i+4 < bptr->length)){
            if ((bptr->data[i] == 'U') && (bptr->data[i+1] == 'R') && (bptr->data[i+2] == 'I') && (bptr->data[i+3] == ':') ){
                URIStart = i+4;
                i = i+4;
            }
        }
        if (URIStart != -1){ URIStop = i; }
        if (bptr->data[i] == ',' || bptr->data[i] == '\r' || bptr->data[i] == '\n' ){
            break;
        }
    }

    if (URIStart == -1 || URIStop == -1 || URIStart >= URIStop ){
        LOGERROR("subjectAltName doesn'r contain URI");
        goto error;
    }

    // copy URI
    URI = malloc( URIStop - URIStart + 1 );
    if (URI == NULL){
        LOGERROR("unable to allocate memory for URI");
        goto error;
    }
    memcpy(URI, &bptr->data[URIStart], URIStop - URIStart);
    URI[URIStop] = '\0';

    LOGDEBUG("subjectAltName %s", URI);

    BIO_free(ext_bio);
    X509_free(cert);

    return URI;
error:
    if(cert != NULL){
        X509_free(cert);
    }
    if(ext_bio != NULL){
        BIO_free(ext_bio);
    }
    if(URI != NULL){
        free(URI);
    }
    return NULL;
}

//------------------------THE ENTRY POINT------------------------------------------------
int main(int argc, char *argv[]) {

    // Create the client object
    opcua_client = UA_Client_new();
    if (opcua_client == NULL){
        LOGERROR("unable to allocate the connection object");
        exit(EXIT_FAILURE);
    }

    OpenSSL_add_all_algorithms();
    ERR_load_BIO_strings();

    SETLOGLEVEL(0);
    LOGINFO("enter eport_loop");
    eport_loop( &on_request );

    return EXIT_SUCCESS;
}