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
#include "opcua_client_protocol.h"
 
cJSON* on_error(char* text);
cJSON* on_ok(cJSON* response);
cJSON* opcua_client_connect(cJSON* request);
cJSON* opcua_client_read(cJSON* request);
cJSON* opcua_client_write(cJSON* request);
cJSON* opcua_client_subscribe(cJSON* request);
cJSON* opcua_client_update_subscriptions(cJSON* request);
cJSON* opcua_client_browse_endpoints(cJSON* request);
cJSON* opcua_client_browse_folder(cJSON* request);

int path2nodeId( cJSON *path, UA_NodeId *node );
char* path2string( cJSON *path );
cJSON* browse_folder( UA_NodeId folder );
cJSON* parse_value( UA_Variant *value );
int export_value(UA_Variant *ua_value, cJSON *value);
int init_subscriptions(void);
static void on_subscription_update(UA_Client *client, UA_UInt32 subId, void *subContext,
                         UA_UInt32 monId, void *monContext, UA_DataValue *value);

UA_ByteString* loadFile(const char* path);
UA_ByteString* parse_base64( char* base64string );
char* parse_certificate_uri( const unsigned char *certificate, size_t len );

// Global variables
UA_Client *opcua_client;
UA_UInt32 subscriptionId;

// Subscription indexes
opcua_client_binding *opcua_client_bindings = NULL;
opcua_client_subscription *opcua_client_subscriptions = NULL;

char* on_request( char *requestString ){
    cJSON *response;
    char *responseString;
    OPCUA_CLIENT_REQUEST request = {};

    // Parse the request
    LOGDEBUG("DEBUG: parsing the request\r\n");
    if (parse_request( requestString, &request ) != 0){
        response = on_error("invalid request");
    }else{
        // Handle the request
        LOGDEBUG("DEBUG: handle the request\r\n");
        if( request.cmd == OPCUA_CLIENT_CONNECT ){
            response = opcua_client_connect( request.body );
        } else if (request.cmd == OPCUA_CLIENT_READ ){
            response = opcua_client_read( request.body );
        }else if (request.cmd == OPCUA_CLIENT_WRITE ){
            response = opcua_client_write( request.body );
        }else if (request.cmd == OPCUA_CLIENT_SUBSCRIBE ){
            response = opcua_client_subscribe( request.body );
        }else if (request.cmd == OPCUA_CLIENT_UPDATE_SUBSCRIPTIONS ){
            response = opcua_client_update_subscriptions( request.body );
        }else if (request.cmd == OPCUA_CLIENT_BROWSE_ENDPOINTS ){
            response = opcua_client_browse_endpoints( request.body );
        }else if (request.cmd == OPCUA_CLIENT_BROWSE_FOLDER ){
            response = opcua_client_browse_folder( request.body );
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

cJSON* opcua_client_connect(cJSON* request){
    cJSON *response = NULL;
    char *errorString = NULL;
    UA_StatusCode retval;

    UA_ClientConfig *config = UA_Client_getConfig(opcua_client);

    // Connection params
    cJSON *url = cJSON_GetObjectItemCaseSensitive(request, "url");
    cJSON *certificate = cJSON_GetObjectItemCaseSensitive(request, "certificate");
    cJSON *privateKey = cJSON_GetObjectItemCaseSensitive(request, "private_key");
    cJSON *login = cJSON_GetObjectItemCaseSensitive(request, "login");
    cJSON *password = cJSON_GetObjectItemCaseSensitive(request, "password");
    UA_ByteString *cert = NULL;
    UA_ByteString *key = NULL;

    char *URI = NULL;

    // Configure the connection
    if (cJSON_IsString(certificate)){
        LOGDEBUG("DEBUG: prepare secure connection\r\n");

        LOGDEBUG("DEBUG: parse base64 certificate\r\n");
        cert = parse_base64( certificate->valuestring );
        if (cert == NULL){
            errorString = "unable to parse the certificate from base64";
            goto error;
        }

        LOGDEBUG("DEBUG: parse base64 key\r\n");
        key = parse_base64( privateKey->valuestring );
        if (key == NULL){
            errorString = "unable to parse the key from base64";
            goto error;
        }

        // Parse the application URI from the certificate
        LOGDEBUG("DEBUG: parse_certificate_uri\r\n");
        char *URI = parse_certificate_uri( (const unsigned char *)cert->data, cert->length );   
        if (URI == NULL){
            errorString = "unable to parse the certificate";
            goto error;
        } 
        LOGDEBUG("DEBUG: application URI: %s\r\n",URI);

        // Trust list
        size_t trustListSize = 0;
        UA_STACKARRAY(UA_ByteString, trustList, trustListSize);

        // Revocation list
        UA_ByteString *revocationList = NULL;
        size_t revocationListSize = 0;

        LOGDEBUG("DEBUG: UA_MESSAGESECURITYMODE_SIGNANDENCRYPT\r\n");

        config->securityMode = UA_MESSAGESECURITYMODE_SIGNANDENCRYPT;
        if (UA_ClientConfig_setDefaultEncryption(config, *cert, *key,
                                            trustList, trustListSize,
                                            revocationList, revocationListSize) 
            != UA_STATUSCODE_GOOD){
            errorString = "unable to configure a secure connection";
            goto error;
        };

        config->clientDescription.applicationUri = UA_STRING_ALLOC(URI);

        free(URI);
        URI = NULL;
    }else{
        LOGDEBUG("DEBUG: unsecure connection\r\n");
        UA_ClientConfig_setDefault(config);
    }

    if (cJSON_IsString(login)){
        // Authorized access
        LOGDEBUG("DEBUG: authorized connection to %s, user %s\r\n", url->valuestring,login->valuestring);
        retval = UA_Client_connectUsername(opcua_client, url->valuestring, login->valuestring, password->valuestring);
    }else{
        LOGDEBUG("DEBUG: anonymous connection to %s\r\n", url->valuestring);
        retval = UA_Client_connect(opcua_client, url->valuestring);
    }

    if(retval != UA_STATUSCODE_GOOD) {
        errorString = "connection error";
        goto error;
    }

    if (init_subscriptions() != 0){
        errorString = "init subscription error";
        goto error;
    }

    response = cJSON_CreateString("ok");
    if (response == NULL){
        goto error;
    }

    return on_ok( response );

error:
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
    if (errorString == NULL){
        errorString = "programming error in opcua_client_connect";
    }
    return on_error( errorString );
}

cJSON* opcua_client_read(cJSON* request){
    char *errorString = NULL;
    UA_Variant *value = UA_Variant_new();
    cJSON *response = NULL;
    LOGDEBUG("DEBUG: reading\r\n");

    UA_NodeId nodeId = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
    int ret = path2nodeId( request, &nodeId );
    if (ret != 0){
        errorString = "invalid node path";
        goto error;
    }

    /* Read the value */
    if (UA_Client_readValueAttribute(opcua_client, nodeId, value) != UA_STATUSCODE_GOOD ) {
        errorString = "unable to read value";
        goto error;
    }

    response = parse_value( value );    
    if (response == NULL){
        errorString = "invalid value";
        goto error;
    }

    UA_Variant_delete(value);

    return on_ok( response );

error:
    UA_Variant_delete(value);
    cJSON_Delete( response );
    if (errorString == NULL){
        errorString = "programming error in opcua_client_read";
    }
    return on_error( errorString );
}

cJSON* opcua_client_write(cJSON* request){
    char *errorString = NULL;
    cJSON *response = NULL;
    UA_Variant *ua_value = UA_Variant_new();
    LOGDEBUG("DEBUG: writing\r\n");

    cJSON *tag = cJSON_GetObjectItemCaseSensitive(request, "tag");
    cJSON *value = cJSON_GetObjectItemCaseSensitive(request, "value");

    // Get the type of the tag
    UA_NodeId nodeId = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
    int ret = path2nodeId( tag, &nodeId );
    if (ret != 0){
        errorString = "invalid node path";
        goto error;
    }
    if (UA_Client_readValueAttribute(opcua_client, nodeId, ua_value) != UA_STATUSCODE_GOOD ) {
        errorString = "unable to define tag type";
        goto error;
    }
    if ( export_value(ua_value, value) != 0 ){
        errorString = "invalid value";
        goto error;
    }

    // Write the value
    if ( UA_Client_writeValueAttribute(opcua_client, nodeId, ua_value) != UA_STATUSCODE_GOOD ){
        errorString = "unable to write value";
        goto error;
    }

    response = cJSON_CreateString("ok");
    if (response == NULL){
        goto error;
    }

    UA_Variant_delete(ua_value);
    
    return on_ok( response );

error:
    UA_Variant_delete( ua_value );
    cJSON_Delete( response );
    if (errorString == NULL){
        errorString = "programming error in opcua_client_write";
    }
    return on_error( errorString );
}

cJSON* opcua_client_subscribe(cJSON* request){
    char *errorString = NULL;
    cJSON *response = NULL;
    char *path = NULL;
    LOGDEBUG("DEBUG: subscribing\r\n");

    // Get the key to the binding
    path = path2string( request );
    if (path == NULL){
        errorString = "unable to convert the node path to string";
        goto error;
    }

    // Lookup the binding in the collection
    opcua_client_binding *b = NULL;
    opcua_client_subscription *s = NULL;
    HASH_FIND_STR(opcua_client_bindings, path, b);

    if (b == NULL){
        // The binding is not in the collection yet.
        // Create a new subscription.
        LOGDEBUG("DEBUG: create a new subscription\r\n");

        // Get nodeId
        UA_NodeId nodeId = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
        int ret = path2nodeId( request, &nodeId );
        if (ret != 0){
            errorString = "invalid node path";
            goto error;
        }

        // Add the binding to the monitored items
        UA_MonitoredItemCreateRequest monRequest = UA_MonitoredItemCreateRequest_default(nodeId);
        UA_MonitoredItemCreateResult monResponse =
        UA_Client_MonitoredItems_createDataChange(opcua_client, subscriptionId, UA_TIMESTAMPSTORETURN_BOTH,
                                                monRequest, NULL, on_subscription_update, NULL);

        LOGDEBUG("DEBUG: monResponse.monitoredItemId %d\r\n", monResponse.monitoredItemId);

        if(monResponse.statusCode != UA_STATUSCODE_GOOD){
            errorString = "uanble to add a monitored item";
            goto error;
        }

        // Add the binding to the collection
        b = (opcua_client_binding *)malloc(sizeof *b);
        if (b == NULL){
            errorString = "uanble to allocate the memory for a new binding index";
            goto error;
        }
        b->path = path;
        b->id = monResponse.monitoredItemId;

        s = (opcua_client_subscription *)malloc(sizeof *s);
        if (s == NULL){
            errorString = "unable to allocate the memory for new binding";
            goto error;
        }
        s->id = monResponse.monitoredItemId;
        s->value = UA_Variant_new();
        
        HASH_ADD_STR(opcua_client_bindings, path, b);
        HASH_ADD_INT(opcua_client_subscriptions, id, s);

        // The subscription is already created, we should not clear the path from this moment
        path = NULL;

        return opcua_client_read( request );
    }else{
        free( path );
        // The binding is already in the active subscriptions
        LOGDEBUG("DEBUG: lookup the value in the active subscriptions\r\n");

        // Lookup the value
        HASH_FIND_INT(opcua_client_subscriptions, &b->id, s);
        if (s == NULL){
            errorString = "invalid subscription index";
            goto error;
        }

        response = parse_value( s->value );
        if (response == NULL){
            errorString = "invalid value";
            goto error;
        }
    }

    return on_ok( response );

error:
    if (path != NULL){
        free(path);
    }
    cJSON_Delete( response );
    if (errorString == NULL){
        errorString = "programming error in opcua_client_read";
    }
    return on_error( errorString );
}

cJSON* opcua_client_update_subscriptions(cJSON* request){
    char *errorString;
    cJSON* response = NULL;

    if (UA_Client_run_iterate(opcua_client, 10) != UA_STATUSCODE_GOOD){
        errorString = "unable update subscriptions";
        goto error;
    }

    response = cJSON_CreateString("ok");
    if (response == NULL){
        errorString = "unable to allocate response";
        goto error;
    }

    return on_ok( response );
error:
    if (errorString == NULL){
        errorString = "programming error in opcua_client_read";
    }
    return on_error( errorString );
}

cJSON* opcua_client_browse_endpoints(cJSON* request){
    UA_Client *client = NULL;
    cJSON *response = NULL;
    cJSON *endpoint = NULL;
    char *errorString = NULL;
    char *connectionString = NULL;

    UA_EndpointDescription* endpointArray = NULL;
    size_t endpointArraySize = 0;
    UA_StatusCode retval;

    // Connection params
    cJSON *host = cJSON_GetObjectItemCaseSensitive(request, "host");
    cJSON *port = cJSON_GetObjectItemCaseSensitive(request, "port");

    // Build the connection string (6 in tail is :<port> as port max string length is 5)
    char *prefix = "opc.tcp://";
    int urlLen = strlen(prefix) + strlen(host->valuestring) + 6; // :65535 is max
    connectionString = malloc( urlLen );
    if (connectionString == NULL){
        errorString = "unable to allocate connectionString";
        goto error;
    }
    sprintf(connectionString, "%s%s:%d", prefix, host->valuestring, (int)port->valuedouble);
    LOGDEBUG("DEBUG: connectionString %s\r\n",connectionString);

    // Create a connection
    client = UA_Client_new();
    if (client == NULL){
        LOGERROR("ERROR: unable to allocate the client\r\n");
        goto error;
    }
    UA_ClientConfig_setDefault(UA_Client_getConfig(client));

    // Request endpoints
    retval = UA_Client_getEndpoints(client, connectionString, &endpointArraySize, &endpointArray);
    if(retval != UA_STATUSCODE_GOOD) {
        errorString = "connection error";
        goto error;
    }
    UA_Client_delete(client);
    LOGDEBUG("DEBUG: %i endpoints found\r\n",(int)endpointArraySize);

    // Build the response
    response = cJSON_CreateArray();
    if(response == NULL){
        errorString = "unable to allocate CJSON object for response";
        goto error;
    }
    for(size_t i=0; i<endpointArraySize; i++) {
        endpoint = cJSON_CreateString( (char *)endpointArray[i].server.discoveryUrls->data );
        if (endpoint == NULL){
            errorString = "unable to allocate CJSON object for endpoint";
            goto error;
        }
        if (!cJSON_AddItemToArray(response,endpoint)){
            errorString = "unable to add endpoint to the array";
            goto error;
        }
    }

    free(connectionString);
    UA_Array_delete(endpointArray,endpointArraySize, &UA_TYPES[UA_TYPES_ENDPOINTDESCRIPTION]);

    return on_ok( response );

error:
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
    if (errorString == NULL){
        errorString = "programming error in opcua_client_browse_endpoints";
    }
    return on_error( errorString );
}

cJSON* opcua_client_browse_folder(cJSON* request){
    cJSON *response = NULL;
    char *errorString = NULL;

    UA_NodeId nodeId = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
    int ret = path2nodeId( request, &nodeId );
    if (ret != 0){
        errorString = "invalid node path";
        goto error;
    }

    response = browse_folder( nodeId );
    if(response == NULL){
        errorString = "invalid folder";
        goto error;
    }

    return on_ok( response );

error:
    cJSON_Delete( response );
    if (errorString == NULL){
        errorString = "programming error in opcua_client_browse_endpoints";
    }
    return on_error( errorString );
}

cJSON* browse_folder( UA_NodeId folder ){
    cJSON *result = NULL;
    cJSON *subitem = NULL;
    UA_String nodeId;

    LOGDEBUG("DEBUG:-----------------browse folder-------------------------\r\n");
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
        LOGERROR("ERROR: UA_Client_Service_browse error\r\n");
        goto error;
    }

    // Build the result
    result = cJSON_CreateObject();
    for(size_t i = 0; i < response.resultsSize; ++i) {
        for(size_t j = 0; j < response.results[i].referencesSize; ++j) {

            UA_ReferenceDescription *ref = &(response.results[i].references[j]);

            // Convert the nodeId to string
            if (UA_NodeId_print(&ref->nodeId.nodeId, &nodeId) != UA_STATUSCODE_GOOD){
                LOGERROR("ERROR: unable to serialize nodeId in browse_folder\r\n");
                goto error; 
            }

            LOGDEBUG("DEBUG: node %s ref->nodeClass %d\r\n",(char *)ref->displayName.text.data,ref->nodeClass);

            subitem = cJSON_CreateObject();
            if(subitem == NULL){
                LOGERROR("ERROR: unable to allocate subitem\r\n");
                goto error;
            }
            
            if (cJSON_AddStringToObject(subitem,"id", (char *)nodeId.data) == NULL) {
                LOGERROR("ERROR: unable to add a nodeId the result in browse_folder\r\n");
                UA_String_clear(&nodeId);
                goto error; 
            }

            if (cJSON_AddNumberToObject(subitem,"type", ref->nodeClass) == NULL) {
                LOGERROR("ERROR: unable to add a type the result in browse_folder\r\n");
                UA_String_clear(&nodeId);
                goto error; 
            }

            if (!cJSON_AddItemToObject(result, (char *)ref->displayName.text.data, subitem)) {
                LOGERROR("ERROR: unable to add a node the result in browse_folder\r\n");
                UA_String_clear(&nodeId);
                goto error; 
            }

            UA_String_clear(&nodeId);
        }
    }
    UA_BrowseRequest_clear(&request);
    UA_BrowseResponse_clear(&response);

    return result;

error:
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

    // Start from the Objects folder
    cJSON_ArrayForEach(level, path) {
        content = browse_folder( *node );
        if (content == NULL){
            LOGERROR("ERROR: path2nodeId unable to browse folder\r\n");
            goto error;
        }

        // Lookup node by name
        next = cJSON_GetObjectItemCaseSensitive(content, level->valuestring );
        if (!cJSON_IsObject(next)){
            LOGERROR("ERROR: path2nodeId invalid level %s\r\n",level->valuestring);
            goto error;
        }
        next = cJSON_GetObjectItemCaseSensitive(next, "id" );
        if (!cJSON_IsString(next) || (next->valuestring == NULL)){
            LOGERROR("ERROR: path2nodeId invalid level %s\r\n",level->valuestring);
            goto error;
        }

        if (UA_NodeId_parse(node, UA_STRING((char*)(uintptr_t)next->valuestring)) != UA_STATUSCODE_GOOD){
            LOGERROR("ERROR: path2nodeId unable to parse nodeId %s\r\n",next->valuestring);
            goto error;
        }
        cJSON_Delete( content );
    }

    return 0;

error:
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
        LOGERROR("ERROR: unable to allocate the memory for path string\r\n");
        goto error;
    }

    // Join
    strcpy(result, "");
    level = NULL;
    cJSON_ArrayForEach(level, path) {
        strcat(result, "/");
        strcat(result, level->valuestring);
    }

    LOGDEBUG("DEBUG: path2string %s\r\n", result);

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
        LOGERROR("ERROR: unable to create a subscription request\r\n");
        goto error;
    }
    LOGDEBUG("DEBUG: Create subscription succeeded, id %u\r\n", subscriptionId);
 
   return 0;
error:
    if(UA_Client_Subscriptions_deleteSingle(opcua_client, subscriptionId) == UA_STATUSCODE_GOOD){
        LOGERROR("ERROR: unable to purge the subscription request\r\n");
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
        LOGDEBUG("DEBUG: update subscription %d\r\n",monId);
        if (UA_Variant_copy( &value->value, s->value ) != UA_STATUSCODE_GOOD){
            LOGERROR("ERROR: unable to copy value on subscription update\r\n");
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
	fread(result->data, result->length, 1, f);
	fclose(f);

	return result;
}

UA_ByteString* parse_base64(char* base64string){
    UA_ByteString *result = UA_ByteString_new();

    LOGDEBUG("DEBUG: allocate UA_String for base64\r\n");
    UA_String b64 = UA_STRING_ALLOC( base64string );

    LOGDEBUG("DEBUG: parse base64 from UA_String\r\n");
    if ( UA_ByteString_fromBase64( result, &b64 ) != UA_STATUSCODE_GOOD){
        LOGERROR("ERROR: unable to parse base64 string\r\n");
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
        LOGERROR("ERROR: unable to parse certificate in memory\r\n");
        goto error;
    }

    // Extract the subjectAltName extension
    int index = X509_get_ext_by_NID( cert, NID_subject_alt_name, -1);
    if (index < 0 ){
        LOGERROR("ERROR: the certificate doesn't have the subjectAltName extension\r\n");
        goto error;
    }
    ex = X509_get_ext(cert, index);
    if (ex == NULL){
        LOGERROR("ERROR: unable to extract subjectAltName extension\r\n");
        goto error;
    }

    // get the extension value
    ext_bio = BIO_new(BIO_s_mem());
    if (ext_bio == NULL){
        LOGERROR("ERROR: unable to allocate memory for extension value BIO\r\n");
        goto error;
    }
    if(!X509V3_EXT_print(ext_bio, ex, 0, 0)){
        LOGERROR("ERROR: unable to allocate memory for extension value BIO\r\n");
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
        LOGERROR("ERROR: subjectAltName doesn'r contain URI\r\n");
        goto error;
    }

    // copy URI
    URI = malloc( URIStop - URIStart + 1 );
    if (URI == NULL){
        LOGERROR("ERROR: unable to allocate memory for URI\r\n");
        goto error;
    }
    memcpy(URI, &bptr->data[URIStart], URIStop - URIStart);
    URI[URIStop] = '\0';

    LOGDEBUG("DEBUG: subjectAltName %s\r\n", URI);

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
        LOGERROR("ERROR: unable to allocate the connection object\r\n");
        exit(EXIT_FAILURE);
    }

    OpenSSL_add_all_algorithms();
    ERR_load_BIO_strings();

    LOGDEBUG("DEBUG: enter eport_loop\r\n");
    eport_loop( &on_request );

    return EXIT_SUCCESS;
}