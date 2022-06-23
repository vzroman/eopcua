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

char *path2nodeId( char *path, UA_NodeId *nodeId );
char *find_in_folder( UA_NodeId folder, char *name, UA_NodeId *nodeId);
UA_BrowseResponse browse_folder(UA_NodeId folder);
char *replace_host(char *URL, char *host);

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
    UA_StatusCode sc;
    UA_Variant *nodeValue = NULL;

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
        LOGDEBUG("create a new subscription");

        // Get nodeId
        UA_NodeId nodeId;
        *error = path2nodeId( path, &nodeId );
        if (*error != NULL){
            goto on_error;
        }

        nodeValue = UA_Variant_new();
        sc = UA_Client_readValueAttribute(opcua_client, nodeId, nodeValue);
        if (sc != UA_STATUSCODE_GOOD ) {
            *error = (char*)UA_StatusCode_name( sc );;
            goto on_error;
        }

        response = ua2json( nodeValue->type, nodeValue->data );
        if (response == NULL){
            *error = "data type is no supported";
            goto on_error;
        }
        // Add the binding to the monitored items
        UA_MonitoredItemCreateRequest monRequest = UA_MonitoredItemCreateRequest_default(nodeId);
        UA_MonitoredItemCreateResult monResponse =
        UA_Client_MonitoredItems_createDataChange(opcua_client, subscriptionId, UA_TIMESTAMPSTORETURN_BOTH,
                                                monRequest, NULL, on_subscription_update, NULL);

        if(monResponse.statusCode != UA_STATUSCODE_GOOD){
            *error = (char*)UA_StatusCode_name( monResponse.statusCode );
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
        s->value = nodeValue;
        
        HASH_ADD_STR(opcua_client_bindings, path, b);
        HASH_ADD_INT(opcua_client_subscriptions, id, s);
        
        return response;
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
    if(nodeValue != NULL){
        UA_Variant_delete(nodeValue);
    } 
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
        sc = UA_Client_writeValueAttribute(opcua_client, s->nodeId, ua_value);
        UA_Variant_delete(ua_value);
        if ( sc != UA_STATUSCODE_GOOD ){
            *error = (char*)UA_StatusCode_name( sc );
            goto on_error;
        }
        return cJSON_CreateString("ok");
    }

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

    if (!cJSON_IsString(args) || (args->valuestring == NULL)){
        *error = "folder is not defined";
        goto on_error; 
    }

    UA_NodeId nodeId;
    *error = path2nodeId( args->valuestring, &nodeId );
    if (*error != NULL){
        goto on_error;
    }

    UA_BrowseResponse ua_response = browse_folder( nodeId );

    if (ua_response.responseHeader.serviceResult != UA_STATUSCODE_GOOD){
        *error = (char*)UA_StatusCode_name( ua_response.responseHeader.serviceResult );
        UA_BrowseResponse_clear(&ua_response);
        goto on_error;
    }
    
    // Build the result
    response = cJSON_CreateObject();
    for(size_t i = 0; i < ua_response.resultsSize; ++i) {
        for(size_t j = 0; j < ua_response.results[i].referencesSize; ++j) {

            UA_ReferenceDescription *ref = &(ua_response.results[i].references[j]);
            
            // Some name can contain '/', it causes the path to be improperly interpretted.
            // We replace them with "\"
            char * name = str_replace( (char *)ref->displayName.text.data, "/", "\\" );
            cJSON *temp = cJSON_AddNumberToObject(response, name, ref->nodeClass);
            free(name);
            if (temp == NULL) {
                *error = "unable to add a node to the result";
                UA_BrowseResponse_clear(&ua_response);
                goto on_error; 
            }
        }
    }

    UA_BrowseResponse_clear(&ua_response);

    return response;

on_error:
    cJSON_Delete( response );
    return NULL;
}


//---------------------------------------------------------------------------
//  Internal helpers
//---------------------------------------------------------------------------
char *path2nodeId( char *path, UA_NodeId *nodeId ){
    char *error = NULL;

    char **tokens = str_split( path, '/');
    // start from the root folder
    *nodeId = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
    if (tokens){
        for (int i = 0; *(tokens + i); i++){
            char *name = *(tokens + i);
            // Some name can contain '/', it causes the path to be improperly interpretted.
            // We replace them with "\"
            name = str_replace( name, "\\", "/" );

            error = find_in_folder( *nodeId, name, nodeId );
            if (error != NULL){
                goto on_error;
            }
        }
        str_split_destroy( tokens );
    }
    
    return NULL;

on_error:
    str_split_destroy( tokens );
    return error;
}

char *find_in_folder( UA_NodeId folder, char *name, UA_NodeId *nodeId){
    char *error = NULL;
    UA_StatusCode sc;

    UA_BrowseResponse response = browse_folder( folder );
    if (response.responseHeader.serviceResult != UA_STATUSCODE_GOOD){
        error = (char*)UA_StatusCode_name( response.responseHeader.serviceResult );
        goto on_error;
    }

    bool found = false;
    for(size_t i = 0; i < response.resultsSize; ++i) {
        for(size_t j = 0; j < response.results[i].referencesSize; ++j) {

            UA_ReferenceDescription *ref = &(response.results[i].references[j]);
            if (strcmp((char *)ref->displayName.text.data, name) == 0){
                sc = UA_NodeId_copy(&ref->nodeId.nodeId, nodeId);
                if (sc != UA_STATUSCODE_GOOD){
                    error = (char*)UA_StatusCode_name( sc );
                }
                found = true;
                break;
            }
        }
        if (found){ break; }
    }
    UA_BrowseResponse_clear(&response);
    if (!found){
        return "node not found";
    }

    return NULL;

on_error:
    UA_BrowseResponse_clear(&response);
    return error;

}

UA_BrowseResponse browse_folder(UA_NodeId folder){
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
    UA_BrowseRequest_clear(&request);
    return response;
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

    LOGINFO("enter eport_loop");
    eport_loop( &on_request );

    pthread_mutex_destroy(&lock);

    return EXIT_SUCCESS;
}