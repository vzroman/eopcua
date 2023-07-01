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
#include <pthread.h>
#include <unistd.h>

#include <open62541/types.h>
#include <open62541/client_config_default.h>
#include <open62541/client_highlevel.h>
#include <open62541/client_subscriptions.h>
#include <open62541/plugin/pki_default.h>

#include "utilities.h"
#include "opcua_client_browse.h"
#include "opcua_client_browse_queue.h"
#include "opcua_client_loop.h"

struct OPCUA_CLIENT {
  UA_Client *client;
  int cycle;
  pthread_mutex_t lock;
  bool run;
} opcua_client;

//-----------------------------------------------------
//  Internal utilities
//-----------------------------------------------------
static void browse_item(char *path, UA_BrowsePath *browsePath){

    char **tokens = str_split( path, '/');
    if (!tokens){
        tokens = malloc( 2 * sizeof(char *));
        tokens[0] = strdup( path );
        tokens[1] = NULL;
    }
    size_t size;
    for (size = 0; tokens[size]; size++);

    UA_BrowsePath_init( browsePath );
    browsePath->startingNode = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
    browsePath->relativePath.elements = (UA_RelativePathElement*)UA_Array_new(size, &UA_TYPES[UA_TYPES_RELATIVEPATHELEMENT]);
    browsePath->relativePath.elementsSize = size;

    for(size_t i = 0; i < size; i++) {
        UA_RelativePathElement *elem = &browsePath->relativePath.elements[i];
        elem->targetName = UA_QUALIFIEDNAME_ALLOC(1, tokens[i]);
    }

    str_split_destroy( tokens );
}

static char *handle_browse_queue(){
    char *error = NULL;
    size_t size;
    char **queue = get_browse_queue(&size);

    if (size && queue == NULL) return "out of memory";
    if (!size) return NULL;

    UA_BrowsePath *browsePath = (UA_BrowsePath*)UA_Array_new(size, &UA_TYPES[UA_TYPES_BROWSEPATH]);
    if (!browsePath) return "out of memory";

    for (size_t i=0; i< size; i++) browse_item( queue[i], &browsePath[i]);

    UA_TranslateBrowsePathsToNodeIdsRequest request;
    UA_TranslateBrowsePathsToNodeIdsRequest_init(&request);
    request.browsePaths = browsePath;
    request.browsePathsSize = size;

    pthread_mutex_lock(&opcua_client.lock);
    UA_TranslateBrowsePathsToNodeIdsResponse response = UA_Client_Service_translateBrowsePathsToNodeIds(opcua_client.client, request);
    pthread_mutex_unlock(&opcua_client.lock);

    if (response.responseHeader.serviceResult != UA_STATUSCODE_GOOD){
        error = (char *)UA_StatusCode_name( response.responseHeader.serviceResult );
        goto on_clear;
    }

    if (response.resultsSize != size){
        error = "unexpected response size";
        goto on_clear;
    }

    for (size_t i=0; i<size; i++){
        if (response.results[i].statusCode != UA_STATUSCODE_GOOD) continue;
        size_t depth = response.results[i].targetsSize;

        UA_NodeId *nodeIdCopy = UA_NodeId_new();
        UA_NodeId_copy(&response.results[i].targets[depth -1].targetId.nodeId, nodeIdCopy);
        
        add_cache( strdup(queue[i]), nodeIdCopy, UA_NODECLASS_VARIABLE );
    }

on_clear:
    UA_Array_delete(browsePath,size,&UA_TYPES[UA_TYPES_BROWSEPATH]);
    UA_TranslateBrowsePathsToNodeIdsResponse_clear(&response);
    purge_browse_queue();
    return error;
}

static void *update_loop_thread(void *arg) {
    LOGINFO("starting the update loop thread");

    char *error;
    UA_StatusCode sc;

    while(opcua_client.run){
        // Wait for the next cycle
        usleep( opcua_client.cycle );

        LOGTRACE("run iterate");
        // get the lock
        pthread_mutex_lock(&opcua_client.lock);

        // Do the update
        sc = UA_Client_run_iterate(opcua_client.client, 0);
        if (sc != UA_STATUSCODE_GOOD){
            error = (char *)UA_StatusCode_name( sc );
        }
        pthread_mutex_unlock(&opcua_client.lock);

        error = handle_browse_queue();
        if (error) LOGERROR("handle browse queue error %s", error);

        if (error){
            LOGERROR("update loop error %s", error);
            break;
        }
    }

    LOGINFO("exit the update loop thread");
    opcua_client.run = false;
    UA_Client_disconnect(opcua_client.client);
    UA_Client_delete(opcua_client.client);
    opcua_client.client = NULL;

    pthread_mutex_destroy(&opcua_client.lock);

    purge_cache();

    return NULL;
}

static char *init_update_loop(int cycle){
    char *error = NULL;

    opcua_client.run = true;
    opcua_client.cycle = cycle ? cycle * 1000 : 100000; // default 100 ms

    // As the open62541 is not thread safe we use mutex
    if (pthread_mutex_init(&opcua_client.lock, NULL)) {
        error = "mutex init has failed";
        goto on_error;
    }

    // The server is going to run in a dedicated thread
    pthread_t updateThread;

    // Launch the client thread
    int res = pthread_create( &updateThread, NULL, &update_loop_thread, NULL);

    if (res !=0 ){
        error = "unable to launch the update loop thread";
        pthread_mutex_destroy(&opcua_client.lock);
        goto on_error;
    }
 
   return error;

on_error:
    opcua_client.run = false;
    return error;
}

static char* check_connected( UA_StatusCode sc ){
    if (sc != UA_STATUSCODE_BADCONNECTIONCLOSED 
    && sc != UA_STATUSCODE_BADCONNECTIONREJECTED
    && sc != UA_STATUSCODE_BADDISCONNECT
    && sc != UA_STATUSCODE_BADMAXCONNECTIONSREACHED
    && sc != UA_STATUSCODE_BADSERVERNOTCONNECTED){
        return (char*)UA_StatusCode_name( sc );
    }else{
        stop();
        return "no connection";
    }
}

//-----------------------------------------------------
//  API
//-----------------------------------------------------
char *start(char *url, char *certificate, char *privateKey, char *login, char *pass, int cycle, size_t maxNodesPerBrowse){
    char *error = NULL;
    UA_StatusCode sc;

    UA_ByteString *cert = NULL;
    UA_ByteString *key = NULL;
    char *appURI = NULL;
    
    if (opcua_client.client) return "already started";

    // Create the client object
    opcua_client.client = UA_Client_new();
    if (!opcua_client.client){
        error = "unable to allocate the connection object";
        goto on_error;
    }

    // get the config object
    UA_ClientConfig *config = UA_Client_getConfig(opcua_client.client);

    // Configure the connection
    if ( certificate ){
        LOGTRACE("prepare secure connection");

        cert = parse_base64( certificate );
        if (cert == NULL){
            error = "unable to parse the certificate from base64";
            goto on_error;
        }

        key = parse_base64( privateKey );
        if (key == NULL){
            error = "unable to parse the key from base64";
            goto on_error;
        }

        // Parse the application URI from the certificate
        appURI = parse_certificate_uri( cert, &error );
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
            error = (char*)UA_StatusCode_name( sc );
            goto on_error;
        };

        UA_CertificateVerification_AcceptAll( &config->certificateVerification );

        config->clientDescription.applicationUri = UA_STRING_ALLOC(appURI);

        free(appURI);
        appURI = NULL;
    }else{
        UA_ClientConfig_setDefault(config);
    }

    if (login){
        // Authorized access
        LOGINFO("authorized connection to %s, user %s", url,login);
        sc = UA_Client_connectUsername(opcua_client.client, url, login, pass);
    }else{
        LOGINFO("anonymous connection to %s", url);
        sc = UA_Client_connect(opcua_client.client, url);
    }
    if(sc != UA_STATUSCODE_GOOD) {
        error = (char*)UA_StatusCode_name( sc );
        goto on_error;
    }

    LOGINFO("build browse cache...");
    error = build_browse_cache( opcua_client.client, maxNodesPerBrowse );
    if (error) goto on_error;

    LOGINFO("enter the update loop");
    error = init_update_loop( cycle );
    if (error) goto on_error;

    return NULL;


on_error:
    if (opcua_client.client){
        UA_Client_disconnect( opcua_client.client );
        UA_Client_delete( opcua_client.client );
    }
    opcua_client.client = NULL;
    opcua_client.run = false;

    if (appURI)free(appURI);
    if (cert)UA_ByteString_delete( cert );
    if (key) UA_ByteString_delete( key );

    return error;
}

void stop(){
    opcua_client.run = false;
}

bool is_started(){
    return opcua_client.run;
}

char* browse_servers(char *host, int port, char ***urls){
    char *error = NULL;
    char **result = NULL;

    UA_Client *client = NULL;
    
    UA_ApplicationDescription *ad = NULL;
    UA_StatusCode sc;


    // Build the connection string (6 in tail is :<port> as port max string length is 5)
    char *protocol = "opc.tcp://";
    char url[ strlen(protocol) + strlen(host) + 6 + 1]; // 6 - "":65535" is max, 1 - \0 
    
    sprintf(url, "%s%s:%d", protocol, host, port);
    LOGDEBUG("connectionString %s",url);

    // Create a connection
    client = UA_Client_new();
    if (!client){
        error = "unable to allocate the client";
        goto on_clear;
    }
    sc = UA_ClientConfig_setDefault(UA_Client_getConfig(client));

    // Request endpoints
    size_t adSize = 0;
    sc = UA_Client_findServers(client, url, 0, NULL, 0, NULL, &adSize, &ad);

    if(sc != UA_STATUSCODE_GOOD) {
        error = (char*)UA_StatusCode_name( sc );
        goto on_clear;
    }

    // Build the result
    int n = 0;
    for(size_t i = 0; i < adSize; i++) {
        for(size_t j = 0; j < ad[i].discoveryUrlsSize; j++) {
            n++;
        }
    }

    result = malloc((n + 1) * sizeof(char*)); // +1 - ending NULL
    if(!result){
        error = "out of memory";
        goto on_clear;
    }

    // Fill in the results
    n = 0;
    for(size_t i = 0; i < adSize; i++) {
        for(size_t j = 0; j < ad[i].discoveryUrlsSize; j++) {
            *(result + n) = strdup((char *)ad[i].discoveryUrls[j].data);
            n++;
        }
    }
    *(result + n) = NULL;

on_clear:
    if (ad) UA_Array_delete(ad, adSize, &UA_TYPES[UA_TYPES_APPLICATIONDESCRIPTION]);
    if(client){
        UA_Client_disconnect(client);
        UA_Client_delete(client);
    }

    if (!error) {
        *urls = result;
        return NULL;
    };

    if (result){
        for (int i = 0; *(result + i); i++){
            free(*(result + i));
        }
        free(result);
    }
    return error;
}

char *read_values(size_t size, UA_NodeId **nodeId, UA_DataValue **values){
    char *error = NULL;

    UA_ReadRequest request;
    UA_ReadRequest_init(&request);

    request.nodesToRead = UA_Array_new(size, &UA_TYPES[UA_TYPES_READVALUEID]);
    if (!request.nodesToRead){
        error = "out of memory";
        goto on_clear;
    }
    for (size_t i=0; i < size; i++){
        UA_ReadValueId_init(&request.nodesToRead[i]);
        UA_NodeId_copy(nodeId[i], &request.nodesToRead[i].nodeId );
        request.nodesToRead[i].attributeId = UA_ATTRIBUTEID_VALUE;
    }
    request.nodesToReadSize = size;    
    
    // Get the lock
    pthread_mutex_lock(&opcua_client.lock);
    UA_ReadResponse response = UA_Client_Service_read(opcua_client.client, request);
    pthread_mutex_unlock(&opcua_client.lock);

    UA_StatusCode sc = response.responseHeader.serviceResult;
    if(sc != UA_STATUSCODE_GOOD) {
        error = check_connected(sc);
        goto on_clear;
    }

    if(response.resultsSize != size){
        error = "invalid response results size";
        goto on_clear;
    }
    
    sc = UA_Array_copy(response.results, size, (void **)values, &UA_TYPES[UA_TYPES_DATAVALUE]);
    if(sc != UA_STATUSCODE_GOOD) {
        error = (char*)UA_StatusCode_name( sc );
        goto on_clear;
    }

on_clear:
    UA_ReadRequest_clear(&request);
    UA_ReadResponse_clear(&response);
    return error;
}

char *write_values(size_t size, UA_NodeId **nodeId, UA_Variant **values, char ***results){
    char *error = NULL;

    UA_WriteRequest request;
    UA_WriteRequest_init(&request);

    request.nodesToWrite = UA_Array_new(size, &UA_TYPES[UA_TYPES_WRITEVALUE]);
    if (!request.nodesToWrite){
        error = "out of memory";
        goto on_clear;
    }
    for (size_t i=0; i < size; i++){
        UA_WriteValue_init(&request.nodesToWrite[i]);
        UA_NodeId_copy(nodeId[i], &request.nodesToWrite[i].nodeId );
        request.nodesToWrite[i].attributeId = UA_ATTRIBUTEID_VALUE;
        request.nodesToWrite[i].value.value = *values[i];
        request.nodesToWrite[i].value.hasValue = true;
    }
    request.nodesToWriteSize = size;

    // Get the lock
    pthread_mutex_lock(&opcua_client.lock);
    UA_WriteResponse response = UA_Client_Service_write(opcua_client.client, request);
    pthread_mutex_unlock(&opcua_client.lock);

    UA_StatusCode sc = response.responseHeader.serviceResult;
    if(sc != UA_STATUSCODE_GOOD) {
        error = check_connected(sc);
        goto on_clear;
    }
    if(response.resultsSize != size){
        error = "invalid response results size";
        goto on_clear;
    }

    char **_results = malloc(size * sizeof(char *));

    for(size_t i=0; i<size; i++){
        if(response.results[i] != UA_STATUSCODE_GOOD) {
            _results[i] = (char *)UA_StatusCode_name( response.results[i] );
        }else{
            _results[i] = NULL;
        }
    }
    *results = _results;

on_clear:
    UA_WriteRequest_clear(&request);
    UA_WriteResponse_clear(&response);
    return error;
}

