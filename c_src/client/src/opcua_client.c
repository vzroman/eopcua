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
#include <unistd.h>
//----------------------------------------
#include <eport_c.h>
//----------------------------------------
#include <open62541/client_config_default.h>
#include <open62541/client_highlevel.h>
#include <open62541/client_subscriptions.h>
#include <open62541/plugin/log_stdout.h>
//----------------------------------------
#include <openssl/x509v3.h>
//----------------------------------------
#include "utilities.h"
#include "opcua_client.h"
   
cJSON* on_request(char* method, cJSON *args, char **error);

cJSON* opcua_client_connect(cJSON* args, char **error);
cJSON* opcua_client_read_items(cJSON* args, char **error);
cJSON* opcua_client_read_item(cJSON* args, char **error);
cJSON* opcua_client_write_items(cJSON* args, char **error);
cJSON* opcua_client_write_item(cJSON* args, char **error);

cJSON* opcua_client_browse_endpoints(cJSON* args, char **error);
cJSON* opcua_client_browse_folder(cJSON* args, char **error);

UA_NodeId path2nodeId( char *path, char **error );
cJSON* browse_folder( UA_NodeId folder, char **error );

//--------------update thread loop-----------------------------
const char *init_update_loop(void);
static void *update_loop_thread(void *arg);
const char *opcua_client_update_subscriptions(void);
static void on_subscription_update(UA_Client *client, UA_UInt32 subId, void *subContext,
                         UA_UInt32 monId, void *monContext, UA_DataValue *value);
UA_StatusCode get_connection_state( UA_Client *client );

// Global variables
UA_Client *opcua_client = NULL;
UA_UInt32 subscriptionId;
int update_interval = 100000; // 100 ms
pthread_mutex_t lock;

// Subscription indexes
opcua_client_binding *opcua_client_bindings = NULL;
opcua_client_subscription *opcua_client_subscriptions = NULL;

cJSON* on_request( char *method, cJSON *args, char **error ){
    
    cJSON *response = NULL;
    // Handle the request
    LOGDEBUG("handle the request %s", method);

    // open62541 is not thread safe, we use mutex
    pthread_mutex_lock(&lock); 

    if( strcmp(method, "connect") == 0){
        response = opcua_client_connect( args, error );
    }else if (strcmp(method, "read_items") == 0){
        response = opcua_client_read_items( args, error );
    }else if (strcmp(method, "read_item") == 0){
        response = opcua_client_read_item( args, error );
    }else if (strcmp(method, "write_items") == 0){
        response = opcua_client_write_items( args, error );
    }else if (strcmp(method, "write_item") == 0){
        response = opcua_client_write_item( args, error );
    }else if (strcmp(method, "browse_endpoints") == 0){
        response = opcua_client_browse_endpoints( args, error );
    }else if (strcmp(method, "browse_folder") == 0){
        response = opcua_client_browse_folder( args, error );
    } else{
        *error = "invalid method";
    }

    // release the lock
    pthread_mutex_unlock(&lock);

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

    UA_ByteString *cert = NULL;
    UA_ByteString *key = NULL;
    char *connectionString = NULL;
    char *URI = NULL;

    if (opcua_client != NULL){
        *error = "already connected";
        goto on_error;
    }
    //-----------validate the arguments-----------------------
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
    opcua_client = UA_Client_new();
    if (opcua_client == NULL){
        *error = "unable to allocate the connection object";
        goto on_error;
    }

    UA_StatusCode retval;

    // get the config object
    UA_ClientConfig *config = UA_Client_getConfig(opcua_client);

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
        URI = parse_certificate_uri( cert, error );   
        if (URI == NULL){
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

    *error = (char *)init_update_loop();
    if (*error != NULL){
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
//  read and write operations always subscribe to the relevant nodes
//  to keep their IDs and other needed attributes in the hash table 
//---------------------------------------------------------------
cJSON* opcua_client_read_items(cJSON* args, char **error){
    LOGTRACE("read items");
    cJSON *response = cJSON_CreateArray();
    cJSON *item = NULL;
    cJSON *result = NULL;

    if (opcua_client == NULL){
        *error = "no connection";
        goto on_error;
    }

    //-----------validate the arguments-----------------------
    if ( !cJSON_IsArray(args) ) {
        *error = "invalid read arguments";
        goto on_error;
    }

    cJSON_ArrayForEach(item, args) {
        result = opcua_client_read_item( item, error );
        if (result == NULL){
            result = cJSON_CreateString("error: read error");
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

cJSON* opcua_client_read_item(cJSON* args, char **error){
    LOGTRACE("read item");

    cJSON *response = NULL;

    if (opcua_client == NULL){
        *error = "no connection";
        goto on_error;
    }

    //-----------validate the arguments-----------------------
    if (!cJSON_IsString(args) || (args->valuestring == NULL)){
        *error = "path is not defined";
        goto on_error; 
    }

    // get item path
    char * path = args->valuestring;

    // Lookup the binding in the collection
    opcua_client_binding *b = NULL;
    opcua_client_subscription *s = NULL;
    HASH_FIND_STR(opcua_client_bindings, path, b);

    if (b == NULL){
        // The binding is not in the collection yet.
        // Create a new subscription.
        LOGTRACE("create a new subscription");

        // Get nodeId
        UA_NodeId nodeId = path2nodeId( path, error );
        if (*error != NULL){
            goto on_error;
        }

        // Add the binding to the monitored items
        UA_MonitoredItemCreateRequest monRequest = UA_MonitoredItemCreateRequest_default(nodeId);
        UA_MonitoredItemCreateResult monResponse =
        UA_Client_MonitoredItems_createDataChange(opcua_client, subscriptionId, UA_TIMESTAMPSTORETURN_BOTH,
                                                monRequest, NULL, on_subscription_update, NULL);

        if(monResponse.statusCode != UA_STATUSCODE_GOOD){
            *error = "uanble to add a monitored item";
            goto on_error;
        }

        LOGDEBUG("monResponse.monitoredItemId %d", monResponse.monitoredItemId);

        // Add the binding to the collection
        b = (opcua_client_binding *)malloc(sizeof *b);
        if (b == NULL){
            *error = "unable to allocate the memory for a new binding index";
            goto on_error;
        }
        b->path = strdup( path );
        b->id = monResponse.monitoredItemId;

        s = (opcua_client_subscription *)malloc(sizeof *s);
        if (s == NULL){
            *error = "unable to allocate the memory for new binding";
            goto on_error;
        }
        s->id = monResponse.monitoredItemId;
        s->nodeId = nodeId;
        
        /* Read the value */
        if (UA_Client_readValueAttribute(opcua_client, nodeId, s->value) != UA_STATUSCODE_GOOD ) {
            *error = "unable to read value";
            goto on_error;
        }
        
        HASH_ADD_STR(opcua_client_bindings, path, b);
        HASH_ADD_INT(opcua_client_subscriptions, id, s);

        response = ua2json( s->value->type, s->value->data );
        if (response == NULL){
            *error = "invalid value";
            goto on_error;
        }
    }else{
        // The binding is already in the active subscriptions
        LOGTRACE("lookup the value in the active subscriptions");

        // Lookup the value
        HASH_FIND_INT(opcua_client_subscriptions, &b->id, s);
        if (s == NULL){
            *error = "invalid subscription index";
            goto on_error;
        }

        response = ua2json( s->value->type, s->value->data );
        if (response == NULL){
            *error = "invalid value";
            goto on_error;
        }
    }

    return response;

on_error:
    cJSON_Delete( response );
    return NULL;
}

//---------------------------------------------------------------
//  WRITE
//---------------------------------------------------------------
cJSON* opcua_client_write_items(cJSON* args, char **error){
    LOGTRACE("write items");
    cJSON *response = cJSON_CreateArray();
    cJSON *item = NULL;
    cJSON *result = NULL;

    if (opcua_client == NULL){
        *error = "no connection";
        goto on_error;
    }

    //-----------validate the arguments-----------------------
    if ( !cJSON_IsArray(args) ) {
        *error = "invalid write_items arguments";
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

    if (opcua_client == NULL){
        *error = "no connection";
        goto on_error;
    }
    //-----------validate the arguments-----------------------
    if ( !cJSON_IsArray(args) ) {
        *error = "invalid write_item arguments";
        goto on_error;
    }

    cJSON *tag = cJSON_GetArrayItem(args, 0);
    if (!cJSON_IsString(tag) || (tag->valuestring == NULL)){
        *error = "item path is not defined";
        goto on_error; 
    }

    cJSON *value = cJSON_GetArrayItem(args, 1);
    if (value == NULL){
        *error = "item value is not defined";
        goto on_error; 
    }

    // Lookup the binding in the collection
    opcua_client_binding *b = NULL;
    opcua_client_subscription *s = NULL;
    HASH_FIND_STR(opcua_client_bindings, tag->valuestring, b);

    if (b == NULL){
        // The item is not in the collection yet, subscribe to it
        response = opcua_client_read_item(tag, error);
        if (response == NULL){
            goto on_error;
        }
        return opcua_client_write_item(args, error);
    }else{
        
        // Lookup the value
        HASH_FIND_INT(opcua_client_subscriptions, &b->id, s);
        if (s == NULL){
            *error = "invalid subscription index";
            goto on_error;
        }

        UA_Variant *ua_value = json2ua(s->value->type, value);
        if ( ua_value == NULL ){
            *error = "invalid value";
            goto on_error;
        }

        // Write the value
        UA_StatusCode status = UA_Client_writeValueAttribute(opcua_client, s->nodeId, ua_value);
        UA_Variant_delete(ua_value);
        if ( status != UA_STATUSCODE_GOOD ){
            *error = (char*)UA_StatusCode_name( status );
            goto on_error;
        }
        return cJSON_CreateString("ok");
    }

on_error:
    cJSON_Delete( response );
    return NULL;
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

    if (opcua_client == NULL){
        *error = "no connection";
        goto on_error;
    }

    if (!cJSON_IsString(args) || (args->valuestring == NULL)){
        *error = "folder is not defined";
        goto on_error; 
    }

    UA_NodeId nodeId = path2nodeId( args->valuestring, error );
    if (*error != NULL){
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
UA_NodeId path2nodeId( char *path, char **error ){
    cJSON *content = NULL;
    cJSON *next = NULL;

    char **tokens = str_split( path, '/');
    // start from the root folder
    UA_NodeId result = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
    if (tokens){
        for (int i = 0; *(tokens + i); i++){
            char *name = *(tokens + i);
            content = browse_folder( result, error );
            if (content == NULL){
                goto on_error;
            }
            // Lookup node by name
            next = cJSON_GetObjectItemCaseSensitive(content, name );
            if (!cJSON_IsObject(next)){
                *error = "invalid node path";
                goto on_error;
            }
            next = cJSON_GetObjectItemCaseSensitive(next, "id" );
            if (!cJSON_IsString(next) || (next->valuestring == NULL)){
                *error = "unable to get node id";
                goto on_error;
            }

            if (UA_NodeId_parse(&result, UA_STRING((char*)(uintptr_t)next->valuestring)) != UA_STATUSCODE_GOOD){
                *error = "unable to parse nodeId";
                goto on_error;
            }
            cJSON_Delete( content );
        }
    }

    str_split_destroy( tokens );
    return result;

on_error:
    str_split_destroy( tokens );
    cJSON_Delete( content );
    if (*error == NULL){
        *error = "invalid node path";
    }
    return result;
}

//------------------------update loop thread----------------------------------
const char *init_update_loop(){
    const char *error = NULL;
    UA_CreateSubscriptionRequest request = UA_CreateSubscriptionRequest_default();
    UA_CreateSubscriptionResponse response = UA_Client_Subscriptions_create(opcua_client, request, NULL, NULL, NULL);
 
    subscriptionId = response.subscriptionId;
    if(response.responseHeader.serviceResult != UA_STATUSCODE_GOOD){
        error = UA_StatusCode_name( response.responseHeader.serviceResult );
        goto on_error;
    }

    // The server is going to run in a dedicated thread
    pthread_t updateThread;

    // Launch the server thread
    int res = pthread_create( &updateThread, NULL, &update_loop_thread, NULL);

    if (res !=0 ){
        error = "unable to launch the update loop thread";
        goto on_error;
    }
 
   return error;
on_error:
    UA_Client_Subscriptions_deleteSingle(opcua_client, subscriptionId);
    return error;
}

static void *update_loop_thread(void *arg) {
    LOGINFO("starting the update loop thread");

    char *error;
    while(true){
        // Wait for the next cycle
        usleep( update_interval );

        LOGTRACE("run iterate");
        // get the lock
        pthread_mutex_lock(&lock);

        // check if the connection is alive
        if (opcua_client == NULL){ 
            pthread_mutex_unlock(&lock);
            break; 
        }

        // Do the update
        error = (char *)opcua_client_update_subscriptions();

        if (error != NULL){
            pthread_mutex_unlock(&lock);
            LOGERROR("update loop error %s", error);
            break;
        }
    }

    LOGINFO("exit the update loop thread");
    if (opcua_client != NULL){
        UA_Client_delete(opcua_client);
        opcua_client = NULL;
    }

    return NULL;
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
            LOGERROR("unable to copy value on subscription update %d",monId);
        }
    }else{
        LOGERROR("unable to update subscription %d",monId);
    }
}

const char *opcua_client_update_subscriptions(){

    UA_StatusCode status = get_connection_state( opcua_client );
    if (status != UA_STATUSCODE_GOOD){
        return UA_StatusCode_name( status );
    };

    status = UA_Client_run_iterate(opcua_client, 100);
    if (status != UA_STATUSCODE_GOOD){
        return UA_StatusCode_name( status );
    }

    return NULL;
}

UA_StatusCode get_connection_state( UA_Client *client ){
    
    const UA_NodeId nodeId = UA_NODEID_NUMERIC(0, UA_NS0ID_SERVER_SERVERSTATUS);

    UA_NodeId dataType;
    UA_NodeId_init(&dataType);

    return UA_Client_readDataTypeAttribute(client, nodeId, &dataType);
}

//------------------------THE ENTRY POINT------------------------------------------------
int main(int argc, char *argv[]) {

    OpenSSL_add_all_algorithms();
    ERR_load_BIO_strings();

    // As the open62541 is not thread safe we use mutex
    if (pthread_mutex_init(&lock, NULL) != 0) {
        LOGINFO("mutex init has failed");
        exit(EXIT_FAILURE);
    }

    SETLOGLEVEL(0);
    LOGINFO("enter eport_loop");
    eport_loop( &on_request );

    return EXIT_SUCCESS;
}