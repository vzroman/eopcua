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

#include <open62541/client_config_default.h>
#include <open62541/client_highlevel.h>
#include <open62541/client_subscriptions.h>

#include "utilities.h"
#include "opcua_client_browse.h"
#include "opcua_client_subscription.h"
#include "opcua_client_loop.h"

struct OPCUA_CLIENT {
  UA_Client *client;
  UA_UInt32 subscriptionId;
  int cycle;
  pthread_mutex_t lock;
  bool run;
} opcua_client;

//-----------------------------------------------------
//  Internal utilities
//-----------------------------------------------------
static UA_StatusCode get_connection_state(){
    
    const UA_NodeId nodeId = UA_NODEID_NUMERIC(0, UA_NS0ID_SERVER_SERVERSTATUS);

    UA_NodeId dataType;
    UA_NodeId_init(&dataType);

    return UA_Client_readDataTypeAttribute(opcua_client.client, nodeId, &dataType);
}

static char *opcua_client_update_subscriptions(){
    UA_StatusCode status = get_connection_state();
    if (status != UA_STATUSCODE_GOOD){
        return (char *)UA_StatusCode_name( status );
    };

    status = UA_Client_run_iterate(opcua_client.client, opcua_client.cycle);
    if (status != UA_STATUSCODE_GOOD){
        return (char *)UA_StatusCode_name( status );
    }

    return NULL;
}

// callback is called by the open62541 client library
static void on_subscription_update(UA_Client *client, UA_UInt32 subId, void *subContext,
                         UA_UInt32 monId, void *monContext, UA_DataValue *value) {
    LOGTRACE("update subscription %d",monId);
    char *error;
    subscription_index *si = NULL;
    monId2subscription(monId, &si);
    if (!si) {
        error = "invalid monId";
        goto on_error;
    } 

    // Update the value
    UA_StatusCode sc = UA_Variant_copy( &value->value, si->value );
    if (sc != UA_STATUSCODE_GOOD){
        error = (char*)UA_StatusCode_name( sc );
        goto on_error;
    }
    if ( value->status != UA_STATUSCODE_GOOD){
        si->status = value->status;
    }else{
        si->status = sc;
    }

    return;                

on_error:
    LOGERROR("unable to update subscription %d: %s",monId, error);
}

static char *register_subscription(char *path){
    LOGINFO("create a new subscription %s",path);

    char *error = NULL;
    
    // Get nodeId
    UA_NodeId nodeId;
    error = path2nodeId( path, &nodeId );
    if (error) goto on_clear;

    UA_NodeId nodeIdCopy;
    UA_StatusCode sc = UA_NodeId_copy( &nodeId, &nodeIdCopy);
    if (sc != UA_STATUSCODE_GOOD){
        error = (char*)UA_StatusCode_name( sc );
        goto on_clear;
    }

    UA_NodeId typeId;
    sc = UA_Client_readDataTypeAttribute(opcua_client.client, nodeId, &typeId);
    if (sc != UA_STATUSCODE_GOOD){
        error = (char*)UA_StatusCode_name( sc );
        goto on_clear;
    }

    UA_Variant *nodeValue = UA_Variant_new();
    UA_StatusCode nodeStatus = UA_Client_readValueAttribute(opcua_client.client, nodeId, nodeValue);

    // Add the binding to the monitored items
    UA_MonitoredItemCreateRequest monRequest = UA_MonitoredItemCreateRequest_default(nodeId);
    UA_MonitoredItemCreateResult monResponse =
    UA_Client_MonitoredItems_createDataChange(opcua_client.client, opcua_client.subscriptionId, UA_TIMESTAMPSTORETURN_BOTH,
                                            monRequest, NULL, on_subscription_update, NULL);

    sc = monResponse.statusCode;
    int monId = monResponse.monitoredItemId;

    UA_MonitoredItemCreateRequest_clear(&monRequest);
    UA_MonitoredItemCreateResult_clear(&monResponse);

    if(sc != UA_STATUSCODE_GOOD){
        error = (char*)UA_StatusCode_name( sc );
        goto on_clear;
    }

    error = add_subscription(path, 
        monId, 
        &nodeIdCopy, 
        nodeValue, 
        nodeStatus, 
        (UA_DataType *)&UA_TYPES[typeId.identifier.numeric - 1]
    );

on_clear:
    return error;
}

static char *get_subscription(char *path,subscription_index **si){
    char *error = path2subscription(path, si);
    if (error){
        // The subscription might be not registered yet
        error = register_subscription( path );
        if (error) return error;

        error = path2subscription(path, si);
    }
    return error;
}

static void *update_loop_thread(void *arg) {
    LOGINFO("starting the update loop thread");

    char *error;
    while(opcua_client.run){
        // Wait for the next cycle
        usleep( opcua_client.cycle );

        LOGTRACE("run iterate");
        // get the lock
        pthread_mutex_lock(&opcua_client.lock);

        // Do the update
        error = opcua_client_update_subscriptions();

        pthread_mutex_unlock(&opcua_client.lock);
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

    purge_subscriptions();
    purge_cache();

    return NULL;
}

static char *init_update_loop(int cycle){
    char *error = NULL;
    UA_CreateSubscriptionRequest request = UA_CreateSubscriptionRequest_default();
    UA_CreateSubscriptionResponse response = UA_Client_Subscriptions_create(opcua_client.client, request, NULL, NULL, NULL);
 
    opcua_client.subscriptionId = response.subscriptionId;
    if(response.responseHeader.serviceResult != UA_STATUSCODE_GOOD){
        error = (char *)UA_StatusCode_name( response.responseHeader.serviceResult );
        goto on_error;
    }

    // As the open62541 is not thread safe we use mutex
    if (pthread_mutex_init(&opcua_client.lock, NULL) != 0) {
        error = "mutex init has failed";
        goto on_error;
    }

    opcua_client.run = true;
    opcua_client.cycle = cycle ? cycle : 100000; // 100 ms

    // The server is going to run in a dedicated thread
    pthread_t updateThread;

    // Launch the client thread
    int res = pthread_create( &updateThread, NULL, &update_loop_thread, NULL);

    if (res !=0 ){
        error = "unable to launch the update loop thread";
        goto on_error;
    }
 
   return error;

on_error:
    opcua_client.run = false;
    pthread_mutex_destroy(&opcua_client.lock);
    UA_Client_Subscriptions_deleteSingle(opcua_client.client, opcua_client.subscriptionId);
    return error;
}

static char *replace_host(char *URL, char *host){

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
    if (!result){
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

//-----------------------------------------------------
//  API
//-----------------------------------------------------
char *start(char *url, char *certificate, char *privateKey, char *login, char *pass, int cycle){
    char *error = NULL;
    UA_StatusCode sc;

    UA_ByteString *cert = NULL;
    UA_ByteString *key = NULL;
    char *appURI = NULL;

    // Create the client object
    opcua_client.client = UA_Client_new();
    if (opcua_client.client == NULL){
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
    error = build_browse_cache( opcua_client.client );
    if (error) goto on_error;

    LOGINFO("enter the update loop");
    error = init_update_loop( cycle );
    if (error) goto on_error;

    return NULL;


on_error:
    if (opcua_client.client){
        UA_Client_disconnect( opcua_client.client );
        UA_Client_delete( opcua_client.client );
        opcua_client.client = NULL;
    }

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
            *(result + n) = replace_host((char *)ad[i].discoveryUrls[j].data, host);
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
    return NULL;
}

char *read_value(char *path, cJSON **value){
    char *error = NULL;
    subscription_index *si = NULL;

    // Get the lock
    pthread_mutex_lock(&opcua_client.lock);

    //  read and write operations always subscribe to the relevant nodes
    //  to keep their IDs and other needed attributes in the hash table 
    error = get_subscription(path, &si);
    if (error) goto on_clear; 

    if (si->status != UA_STATUSCODE_GOOD){
        error = (char*)UA_StatusCode_name( si->status );
        goto on_clear;
    }

    *value = ua2json( si->value->type, si->value->data );
    if (*value == NULL) error = "invalid value";

on_clear:
    pthread_mutex_unlock(&opcua_client.lock);
    return error;
}

char *write_value(char *path, cJSON *value){
    char *error = NULL;
    subscription_index *si = NULL;

    // Get the lock
    pthread_mutex_lock(&opcua_client.lock); 

    //  read and write operations always subscribe to the relevant nodes
    //  to keep their IDs and other needed attributes in the hash table 
    error = get_subscription(path, &si);
    if(error) goto on_clear;

    UA_DataType *type = si->type;
    if (si->value && si->value->type){
        type = (UA_DataType *)si->value->type;
    }

    UA_Variant *ua_value = json2ua(type, value);
    if ( ua_value == NULL ){
        error = "invalid value";
        goto on_clear;
    }

    // Write the value
    UA_StatusCode sc = UA_Client_writeValueAttribute(opcua_client.client, si->nodeId, ua_value);
    UA_Variant_delete(ua_value);
    if ( sc != UA_STATUSCODE_GOOD ) error = (char*)UA_StatusCode_name( sc );

on_clear:
    pthread_mutex_unlock(&opcua_client.lock);
    return error;
}

