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

cJSON* opcua_client_browse_servers(cJSON* args, char **error);
cJSON* opcua_client_browse_folder(cJSON* args, char **error);
cJSON* opcua_client_find_recursive(cJSON* args, char **error);

opcua_client_subscription *find_binding(char *path, char **error);
char *path2nodeId( char *path, UA_NodeId *nodeId );
RefArray browse_folder(UA_NodeId folder, u_int offset, u_int limit, char **error);
char *build_browse_cache(void);
char *build_browse_cache_inner(RefArray *folders);
char *replace_host(char *URL, char *host);

char *initRefArray(RefArray *a, size_t initialSize);
char *insertRefArray(RefArray *a, UA_ReferenceDescription element);
void freeRefArray(RefArray *a);


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

cJSON* on_request( char *method, cJSON *args, char **error ){
    
    cJSON *response = NULL;
    // Handle the request
    LOGTRACE("handle the request %s", method);

    // open62541 is not thread safe, we use mutex
    pthread_mutex_lock(&lock); 

    if (strcmp(method, "browse_servers") == 0){
        response = opcua_client_browse_servers( args, error );
    }else if( strcmp(method, "connect") == 0){
        response = opcua_client_connect( args, error );
    }else if (strcmp(method, "read_items") == 0){
        response = opcua_client_read_items( args, error );
    }else if (strcmp(method, "read_item") == 0){
        response = opcua_client_read_item( args, error );
    }else if (strcmp(method, "write_items") == 0){
        response = opcua_client_write_items( args, error );
    }else if (strcmp(method, "write_item") == 0){
        response = opcua_client_write_item( args, error );
    }else if (strcmp(method, "browse_folder") == 0){
        response = opcua_client_browse_folder( args, error );
    }else if (strcmp(method, "find_recursive") == 0){
        response = opcua_client_find_recursive( args, error );
    } else{
        *error = "invalid method";
    }

    // release the lock
    pthread_mutex_unlock(&lock);

    return response;
}

//---------------------------------------------------------------
//  Servers discovery
//---------------------------------------------------------------
cJSON* opcua_client_browse_servers(cJSON* args, char **error){
    UA_Client *client = NULL;
    cJSON *response = NULL;
    cJSON *host = NULL;
    cJSON *port = NULL;
    char *connectionString = NULL;
    UA_ApplicationDescription *ad = NULL;
    size_t adSize = 0;
    UA_StatusCode sc;

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
    sc = UA_Client_findServers(client, connectionString, 0, NULL, 0, NULL, &adSize, &ad);

    // The client is not needed anymore                                   
    free(connectionString);
    connectionString = NULL;
    UA_Client_disconnect(client); 
    UA_Client_delete(client); 
    client = NULL;

    if(sc != UA_STATUSCODE_GOOD) {
        *error = (char*)UA_StatusCode_name( sc );
        goto on_error;
    }

    // Build the response
    response = cJSON_CreateArray();
    if(response == NULL){
        *error = "unable to allocate CJSON object for response";
        goto on_error;
    }

    for(size_t i = 0; i < adSize; i++) {
        UA_ApplicationDescription *d = &ad[i];
        for(size_t j = 0; j < d->discoveryUrlsSize; j++) {

            char *dURL = replace_host((char *)d->discoveryUrls[j].data, host->valuestring);
            cJSON *URL = cJSON_CreateString( dURL );
            free(dURL);

            if (URL == NULL){
                *error = "unable to allocate CJSON object for URL";
                goto on_error;
            }
            if (!cJSON_AddItemToArray(response,URL)){
                *error = "unable to add endpoint to the array";
                goto on_error;
            }
        }
    }

    UA_Array_delete(ad,adSize, &UA_TYPES[UA_TYPES_APPLICATIONDESCRIPTION]);
    ad = NULL;

    return response;

on_error:
    if (connectionString != NULL){
        free(connectionString);
    }
    if (ad != NULL){
        UA_Array_delete(ad, adSize, &UA_TYPES[UA_TYPES_ENDPOINTDESCRIPTION]);
    }
    if(client != NULL){
        UA_Client_disconnect(client);
        UA_Client_delete(client);
    }
    cJSON_Delete( response );
    return NULL;
}

// The expected structure is:
//     {
//         "url": "opc.tcp://192.168.1.88:53530/OPCUA/SimulationServer",
//         ----optional---------
//         "certificate": "<base64 encoded der>",
//         "privateKey": "<base64 encoded pem>",
//         "login":"user1",
//         "password":"secret",
//         "update_cycle":200
//     }
cJSON* opcua_client_connect(cJSON* args, char **error){
    cJSON *response = NULL;

    cJSON *url = NULL;
    cJSON *update_cycle = NULL;
    cJSON *login = NULL;
    cJSON *certificate = NULL;
    cJSON *privateKey = NULL;
    cJSON *password = NULL;

    UA_ByteString *cert = NULL;
    UA_ByteString *key = NULL;
    char *appURI = NULL;

    UA_StatusCode sc;

    if (opcua_client != NULL){
        *error = "already connected";
        goto on_error;
    }
    //-----------validate the arguments-----------------------
    url = cJSON_GetObjectItemCaseSensitive(args, "url");
    if (!cJSON_IsString(url) || (url->valuestring == NULL)){
        *error = "url is not defined";
        goto on_error; 
    }

    update_cycle = cJSON_GetObjectItemCaseSensitive(args, "update_cycle");
    if (cJSON_IsNumber(update_cycle)){
        update_interval = update_cycle->valuedouble * 1000; 
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

    //--------------Connecting procedure------------------------------
    opcua_client = UA_Client_new();
    if (opcua_client == NULL){
        *error = "unable to allocate the connection object";
        goto on_error;
    }

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
        appURI = parse_certificate_uri( cert, error );
        if (appURI == NULL){
            goto on_error;
        } 

        // Trust list
        size_t trustListSize = 0;
        UA_STACKARRAY(UA_ByteString, trustList, trustListSize);

        // Revocation list
        UA_ByteString *revocationList = NULL;
        size_t revocationListSize = 0;

        config->securityMode = UA_MESSAGESECURITYMODE_SIGNANDENCRYPT;
        sc = UA_ClientConfig_setDefaultEncryption(config, *cert, *key,
                                            trustList, trustListSize,
                                            revocationList, revocationListSize);

        if (sc != UA_STATUSCODE_GOOD){
            *error = (char*)UA_StatusCode_name( sc );
            goto on_error;
        };

        config->clientDescription.applicationUri = UA_STRING_ALLOC(appURI);

        free(appURI);
        appURI = NULL;
    }else{
        UA_ClientConfig_setDefault(config);
    }

    if (cJSON_IsString(login)){
        // Authorized access
        LOGINFO("authorized connection to %s, user %s", url->valuestring,login->valuestring);
        sc = UA_Client_connectUsername(opcua_client, url->valuestring, login->valuestring, password->valuestring);
    }else{
        LOGINFO("anonymous connection to %s", url->valuestring);
        sc = UA_Client_connect(opcua_client, url->valuestring);
    }

    if(sc != UA_STATUSCODE_GOOD) {
        *error = (char*)UA_StatusCode_name( sc );
        goto on_error;
    }

    LOGINFO("build browse cache...");
    *error = build_browse_cache();
    if (*error) goto on_error;

    LOGINFO("enter the update loop");
    *error = (char *)init_update_loop();
    if (*error != NULL){
        goto on_error;
    }

    return cJSON_CreateString("ok");

on_error:
    if (opcua_client != NULL){
        UA_Client_disconnect(opcua_client);
        UA_Client_delete(opcua_client);
        opcua_client = NULL;
    }

    if (appURI != NULL){
        free(appURI);
    }
    if (cert!= NULL){
        UA_ByteString_delete( cert );
    }
    if (key!= NULL){
        UA_ByteString_delete( key );
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
            char _error[strlen(*error) + strlen("error: ") + 1];
            sprintf(_error,"error: %s",*error); 
            result = cJSON_CreateString(_error);
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
    opcua_client_subscription *s = find_binding(path, error);
    if (s == NULL) goto on_error;

    if (s->status != UA_STATUSCODE_GOOD){
        *error = (char*)UA_StatusCode_name( s->status );
        goto on_error;
    }

    response = ua2json( s->value->type, s->value->data );
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
            char _error[strlen(*error) + strlen("error: ") + 1];
            sprintf(_error,"error: %s",*error); 
            result = cJSON_CreateString(_error);
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
    UA_StatusCode sc;

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
    opcua_client_subscription *s = find_binding(tag->valuestring, error);
    if (s == NULL) goto on_error;

    UA_DataType *type = s->type;
    if (s->value && s->value->type){
        type = (UA_DataType *)s->value->type;
    }

    UA_Variant *ua_value = json2ua(type, value);
    if ( ua_value == NULL ){
        *error = "invalid value";
        goto on_error;
    }

    // Write the value
    sc = UA_Client_writeValueAttribute(opcua_client, s->nodeId, ua_value);
    UA_Variant_delete(ua_value);
    if ( sc != UA_STATUSCODE_GOOD ){
        *error = (char*)UA_StatusCode_name( sc );
        goto on_error;
    }
    return cJSON_CreateString("ok");
    

on_error:
    cJSON_Delete( response );
    return NULL;
}

cJSON* opcua_client_browse_folder(cJSON* args, char **error){
    cJSON *response = NULL;

    if (opcua_client == NULL){
        *error = "no connection";
        goto on_error;
    }

    if (!cJSON_IsObject(args)){
        *error = "invalid arguments format";
        goto on_error;
    }

    char *path = "";
    cJSON *_path = cJSON_GetObjectItemCaseSensitive(args, "path");
    if (cJSON_IsString(_path) && (_path->valuestring != NULL)){
        path = _path->valuestring;
    }

    u_int offset = 0;
    cJSON *_offset = cJSON_GetObjectItemCaseSensitive(args, "offset");
    if (cJSON_IsNumber(_offset)){
        offset = _offset->valueint;
    }

    u_int limit = 0;
    cJSON *_limit = cJSON_GetObjectItemCaseSensitive(args, "limit");
    if (cJSON_IsNumber(_limit)){
        limit = _limit->valueint;
    }

    UA_NodeId nodeId;
    *error = path2nodeId( path, &nodeId );
    if (*error != NULL){
        goto on_error;
    }

    RefArray items = browse_folder( nodeId, offset, limit, error );
    if (!items.array) goto on_error;
    
    // Build the result
    response = cJSON_CreateObject();
    for(size_t i = 0; i < items.used; ++i) {

        UA_ReferenceDescription *ref = &items.array[i];
            
        // Some name can contain '/', it causes the path to be improperly interpretted.
        // We replace them with "\"
        char * name = str_replace( (char *)ref->displayName.text.data, "/", "\\" );
        cJSON *temp = cJSON_AddNumberToObject(response, name, ref->nodeClass);
        free(name);
        if (temp == NULL) {
            freeRefArray(&items);
            *error = "unable to add a node to the result";
            goto on_error; 
        }
    }
    freeRefArray(&items);

    return response;

on_error:
    cJSON_Delete( response );
    return NULL;
}

cJSON* opcua_client_find_recursive(cJSON* args, char **error){
    cJSON *response = NULL;

    if (opcua_client == NULL){
        *error = "no connection";
        goto on_error;
    }

    if (!cJSON_IsString(args) || (args->valuestring == NULL)){
        *error = "undefined search string";
        goto on_error;
    }

    char *search = args->valuestring;;

    response = cJSON_CreateArray();
    if (!response){
        *error = "unable to create result set";
        goto on_error;
    }

    char **items = get_all_cache_items();
    for (int i = 0; *(items + i); i++){
        char *item = *(items + i);
        LOGINFO("DEBUG: item %s", item);
        if (strstr(item, search)){
            if (!cJSON_AddItemToArray(response, cJSON_CreateString( item ))){
                *error = "unable to add item to the array";
                goto on_error;
            }
        }
    }
    free(items);

    return response;

on_error:
    cJSON_Delete( response );
    return NULL;
}


//---------------------------------------------------------------------------
//  Internal helpers
//---------------------------------------------------------------------------
opcua_client_subscription *find_binding(char *path, char **error){

    // Lookup the binding in the collection
    opcua_client_subscription *s = find_subscription(path, error);
    if (*error) goto on_error;

    if (!s){
        // The binding is not in the collection yet.
        // Create a new subscription.
        LOGINFO("create a new subscription %s",path);

        // Get nodeId
        UA_NodeId nodeId;
        *error = path2nodeId( path, &nodeId );
        if (*error != NULL){
            goto on_error;
        }

        UA_NodeId nodeIdCopy;
        UA_StatusCode sc = UA_NodeId_copy( &nodeId, &nodeIdCopy);
        if (sc != UA_STATUSCODE_GOOD){
            *error = (char*)UA_StatusCode_name( sc );
            goto on_error;
        }

        UA_NodeId typeId;
        sc = UA_Client_readDataTypeAttribute(opcua_client, nodeId, &typeId);
        if (sc != UA_STATUSCODE_GOOD){
            *error = (char*)UA_StatusCode_name( sc );
            goto on_error;
        }

        UA_Variant *nodeValue = UA_Variant_new();
        UA_StatusCode nodeStatus = UA_Client_readValueAttribute(opcua_client, nodeId, nodeValue);

        // Add the binding to the monitored items
        UA_MonitoredItemCreateRequest monRequest = UA_MonitoredItemCreateRequest_default(nodeId);
        UA_MonitoredItemCreateResult monResponse =
        UA_Client_MonitoredItems_createDataChange(opcua_client, subscriptionId, UA_TIMESTAMPSTORETURN_BOTH,
                                                monRequest, NULL, on_subscription_update, NULL);

        sc = monResponse.statusCode;
        int monId = monResponse.monitoredItemId;

        UA_MonitoredItemCreateRequest_clear(&monRequest);
        UA_MonitoredItemCreateResult_clear(&monResponse);

        if(sc != UA_STATUSCODE_GOOD){
            *error = (char*)UA_StatusCode_name( sc );
            goto on_error;
        }

        s = add_subscription(path, 
            monId, 
            &nodeIdCopy, 
            nodeValue, 
            nodeStatus, 
            (UA_DataType *)&UA_TYPES[typeId.identifier.numeric - 1],
            error
        );
    }
    
    return s;

on_error:
    return NULL;
}

char *path2nodeId( char *path, UA_NodeId *nodeId ){
   
    // Cached version
    UA_NodeId *cached = lookup_path2nodeId_cache( path );
    if (!cached){
        return "invalid node";
    }
    *nodeId = *cached;
    return NULL;
}

//-----------------browse folder utilities-------------------------------
RefArray browse_folder(UA_NodeId folder, u_int offset, u_int limit, char **error){
    
    RefArray result;

    // Build the request
    UA_BrowseRequest request;
    UA_BrowseRequest_init(&request);

    UA_BrowseResponse response;
    UA_BrowseResponse_init(&response);

    UA_BrowseNextRequest nextRequest;
    UA_BrowseNextRequest_init(&nextRequest);

    UA_BrowseNextResponse nextResponse;
    UA_BrowseNextResponse_init(&nextResponse);

    UA_ByteString continuation;
    UA_ByteString_init(&continuation);

    // The batches by 500 items
    const u_int maxPerRequest = 500; 
    *error = initRefArray(&result, maxPerRequest);
    if (*error) goto on_clear;

    // Configure the request
    request.requestedMaxReferencesPerNode = maxPerRequest;    
    request.nodesToBrowse = UA_BrowseDescription_new();
    request.nodesToBrowseSize = 1;
    request.nodesToBrowse[0].nodeId = folder; 
    request.nodesToBrowse[0].resultMask = UA_BROWSERESULTMASK_DISPLAYNAME | UA_BROWSERESULTMASK_NODECLASS;

    response = UA_Client_Service_browse(opcua_client, request);

    if (response.responseHeader.serviceResult != UA_STATUSCODE_GOOD){
        *error = (char*)UA_StatusCode_name( response.responseHeader.serviceResult );
        goto on_clear;
    }

    // The iterator
    u_int iter = 0;

    u_int count = 0;
    for(size_t i = 0; i < response.resultsSize; ++i) {
        for(size_t j = 0; j < response.results[i].referencesSize; ++j) {
            count++;

            // Take only folders and variables
            if(response.results[i].references[j].nodeClass != UA_NODECLASS_OBJECT 
                && response.results[i].references[j].nodeClass != UA_NODECLASS_VARIABLE)
                continue;

            if (++iter <= offset) continue;
            
            // Add a copy of the reference
            *error = insertRefArray(&result, response.results[i].references[j]);
            if (*error) goto on_clear;

            if (limit && result.used >= limit) goto on_clear;
        }
    }

    // There are no more nodes to browse
    if ( count < maxPerRequest ) goto on_clear;

    // Load step by step other nodes
    nextRequest.continuationPointsSize=0;
    UA_ByteString_copy(&response.results[0].continuationPoint, &continuation);
    for(;;){
        nextRequest.continuationPoints = NULL;
        nextRequest.continuationPointsSize = 0;
        UA_BrowseNextRequest_clear(&nextRequest);
        UA_BrowseNextResponse_clear(&nextResponse);

        nextRequest.releaseContinuationPoints = UA_FALSE;
        nextRequest.continuationPoints = &continuation;
        nextRequest.continuationPointsSize=1 ;

        nextResponse = UA_Client_Service_browseNext(opcua_client, nextRequest);

        if (nextResponse.responseHeader.serviceResult != UA_STATUSCODE_GOOD){
            *error = (char*)UA_StatusCode_name( nextResponse.responseHeader.serviceResult );
            goto on_clear;
        }

        count = 0;
        for(size_t i = 0; i < nextResponse.resultsSize; ++i) {
            for(size_t j = 0; j < nextResponse.results[i].referencesSize; ++j) {
                count++;

                // Take only folders and variables
                if(nextResponse.results[i].references[j].nodeClass != UA_NODECLASS_OBJECT 
                    && nextResponse.results[i].references[j].nodeClass != UA_NODECLASS_VARIABLE)
                    continue;

                if (++iter <= offset) continue;
                // Add a copy of the reference
                *error = insertRefArray(&result, nextResponse.results[i].references[j]);
                if (*error) goto on_clear;

                if (limit && result.used >= limit) goto on_clear;
            }
        }
        // No more results
        if (count < maxPerRequest) goto on_clear;
        UA_ByteString_clear(&continuation);
        UA_ByteString_copy(&nextResponse.results[0].continuationPoint, &continuation);
    }


on_clear:
    // The procedure is taken from:
    //      open62541/tests/client/check_client_highlevel.c

    // Release continuation points
    UA_BrowseNextResponse_clear(&nextResponse);
    nextRequest.releaseContinuationPoints = UA_TRUE;
    nextResponse = UA_Client_Service_browseNext(opcua_client, nextRequest);
    UA_BrowseNextResponse_clear(&nextResponse);

    // Clear the main request/response
    UA_BrowseRequest_clear(&request);
    UA_BrowseResponse_clear(&response);

    // Clear the nextRequest
    nextRequest.continuationPoints = NULL;
    nextRequest.continuationPointsSize = 0;
    UA_BrowseNextRequest_clear(&nextRequest);

    UA_ByteString_clear(&continuation);

    if (*error) goto on_error;

    return result;

on_error:
    freeRefArray(&result);
    return result;
}


char *build_browse_cache(){
    char *error;
    UA_NodeId root = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);

    LOGINFO("DEBUG: browse root");
    RefArray items = browse_folder(root, 0, 0, &error);
    LOGINFO("DEBUG: browse root finish");
    if(error) goto on_clear;

    LOGINFO("DEBUG: browse root %lu",items.used);
    // Found in the root subfolders
    RefArray subfolders;
    subfolders.used = 0;
    error = initRefArray(&subfolders, 500);
    LOGINFO("DEBUG: initRefArray(&subfolders, 500) %s",error);
    if (error) goto on_clear;

    for(size_t i = 0; i < items.used; ++i) {

        UA_ReferenceDescription *ref = &items.array[i];

        char * name = (char *)ref->displayName.text.data;

        LOGINFO("DEBUG: add_cache: %s",name);
        error = add_cache(name, &ref->nodeId.nodeId );
        if (error) goto on_clear; 

        // Add children recursively
        if(ref->nodeClass == UA_NODECLASS_OBJECT){
            LOGINFO("DEBUG: build_browse_cache folder: %s",name);
            error = insertRefArray(&subfolders, *ref);
            if (error) goto on_clear;
        }
    }

    if (subfolders.used){
        error = build_browse_cache_inner( &subfolders );
        if (error) goto on_clear;
    }

on_clear:
    freeRefArray( &items );
    freeRefArray( &subfolders );
    return error;
}

char *build_browse_cache_inner(RefArray *folders){

    char *error = NULL;

    RefArray subfolders;
    subfolders.used = 0;
    error = initRefArray(&subfolders, 500);
    if (error) goto on_clear;

    // Build the request
    UA_BrowseRequest request;
    UA_BrowseRequest_init(&request);

    UA_BrowseResponse response;
    UA_BrowseResponse_init(&response);

    // Configure the request
    request.requestedMaxReferencesPerNode = 0;    

    request.nodesToBrowse = UA_Array_new(folders->used, &UA_TYPES[UA_TYPES_BROWSEDESCRIPTION]);
    request.nodesToBrowseSize = folders->used;

    for(size_t i = 0; i < folders->used; ++i) {
        request.nodesToBrowse[i].nodeId = folders->array[i].nodeId.nodeId; 
        request.nodesToBrowse[i].resultMask = UA_BROWSERESULTMASK_DISPLAYNAME | UA_BROWSERESULTMASK_NODECLASS;
    }

    LOGINFO("DEBUG : UA_Client_Service_browse inner");
    response = UA_Client_Service_browse(opcua_client, request);
    LOGINFO("DEBUG : UA_Client_Service_browse inner finish");

    if (response.responseHeader.serviceResult != UA_STATUSCODE_GOOD){
        error = (char*)UA_StatusCode_name( response.responseHeader.serviceResult );
        goto on_clear;
    }

    for(size_t i = 0; i < response.resultsSize; ++i) {
        for(size_t j = 0; j < response.results[i].referencesSize; ++j) {

            UA_ReferenceDescription *ref = &response.results[i].references[j];

            // Take only folders and variables
            if(ref->nodeClass != UA_NODECLASS_OBJECT && ref->nodeClass != UA_NODECLASS_VARIABLE)
                continue;

            char *name = (char *)ref->displayName.text.data;
            char *context = lookup_nodeId2path_cache( folders->array[i].nodeId.nodeId );
            if (!context) {
                error = "unable to find path by nodeId";
                goto on_clear;
            }

            char path[strlen(context) + strlen(name) + 2];
            sprintf(path,"%s/%s",context, name);

            LOGINFO("DEBUG: add_cache: %s",path);
            error = add_cache(path, &ref->nodeId.nodeId );
            if (error) goto on_clear; 

            // Add children recursively
            if(ref->nodeClass == UA_NODECLASS_OBJECT){
                LOGINFO("DEBUG: build_browse_cache: %s",path);
                error = insertRefArray(&subfolders, *ref);
                if (error) goto on_clear;
            }
        }
    }

    if (subfolders.used){
        error = build_browse_cache_inner( &subfolders );
        if (error) goto on_clear;
    }

on_clear:

    // Clear the main request/response
    UA_BrowseRequest_clear(&request);
    UA_BrowseResponse_clear(&response);

    freeRefArray( &subfolders );

    return error;
}

char *replace_host(char *URL, char *host){

    // opc.tcp://fp-roman:53530/OPCUA/SimulationServer

    char *hostStart = strstr(URL, "//");
    if (hostStart == NULL){
        // Is it possible? Just return the original URL
        return strdup(URL);
    }
    // The point where the host starts
    hostStart += 2;

    // The point where the ports starts
    char *tail = strstr(hostStart, ":");
    if (tail == NULL){
        // Is it possible? Just return the original URL
        return strdup(URL);
    }

    size_t prefixLength = hostStart - URL;
    size_t hostLength = strlen( host );
    size_t tailLength = strlen( tail );
    size_t resultLength = prefixLength + hostLength + tailLength + 1;
    
    char *result = (char *)malloc( resultLength ); // allocate the memory for the result
    if (result == NULL){
        LOGERROR("unable to allocate a memory for the result");
        return strdup(URL);
    }

    // Copy the protocol
    strncpy(result, URL, prefixLength);

    // Copy the host 
    strncpy(result + prefixLength, host, hostLength);

    // Copy the tail
    strncpy(result + prefixLength + hostLength, tail, tailLength + 1);

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

        pthread_mutex_unlock(&lock);
        if (error != NULL){
            LOGERROR("update loop error %s", error);
            break;
        }
    }

    LOGINFO("exit the update loop thread");
    if (opcua_client != NULL){
        UA_Client_disconnect(opcua_client);
        UA_Client_delete(opcua_client);
        opcua_client = NULL;
        purge_subscriptions();
        purge_cache();
    }

    return NULL;
}

static void on_subscription_update(UA_Client *client, UA_UInt32 subId, void *subContext,
                         UA_UInt32 monId, void *monContext, UA_DataValue *value) {

    LOGTRACE("update subscription %d",monId);                         
    char *error = update_subscription(monId, value);
    if (error) LOGERROR("%s %d",error, monId);
    
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

    LOGINFO("enter eport_loop");
    eport_loop( &on_request );

    pthread_mutex_destroy(&lock);

    return EXIT_SUCCESS;
}