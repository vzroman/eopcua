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
 
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <eport.h>
#include "opcua_server_protocol.h"

int parse_connect_request( cJSON *body );
int parse_read_request( cJSON *body );
int parse_write_request( cJSON *body );
int parse_subscribe_request( cJSON *body );
int parse_update_subscriptions_request( cJSON *body );
int parse_browse_endpoints_request( cJSON *body );
int parse_browse_folder_request( cJSON *body );

OPCUA_CLIENT_CMD string2cmd(char *cmd);
char* cmd2string(OPCUA_CLIENT_CMD cmd);


int parse_request( const char *message, OPCUA_CLIENT_REQUEST* request ){

    cJSON *JSON = NULL;
    const cJSON *cmd = NULL;
    const cJSON *tid = NULL;
    request->body = NULL;

    // Parse the request to JSON structure
    LOGDEBUG("DEBUG: parse JSON\r\n");
    JSON = cJSON_Parse( message );
    if (JSON == NULL){
        const char *error_ptr = cJSON_GetErrorPtr();
        if (error_ptr != NULL) {
            LOGERROR("ERROR: invalid JSON before: %s\r\n", error_ptr);
        }else{
            LOGERROR("ERROR: invalid JSON\r\n");
        }
        goto error;
    }

    // Parse the type of the request
    LOGDEBUG("DEBUG: parse cmd\r\n");
    cmd = cJSON_GetObjectItemCaseSensitive(JSON, "cmd");
    if (cJSON_IsString(cmd) && (cmd->valuestring != NULL)){
        request->cmd = string2cmd( cmd->valuestring );
        if (request->cmd == -1){
            LOGERROR("ERROR: invalid command type: %s\r\n", cmd->valuestring);
            goto error;
        }
    } else {
        LOGERROR("ERROR: command type is not defined\r\n");
        goto error;
    }

    // Parse transaction ID
    LOGDEBUG("DEBUG: parse tid\r\n");
    tid = cJSON_GetObjectItemCaseSensitive(JSON, "tid");
    if ( cJSON_IsNumber(tid)) {
        request->tid = tid->valuedouble;
    } else{
        LOGERROR("ERROR: transaction id not defined\r\n");
        goto error;
    }
    
    // Parse body
    LOGDEBUG("DEBUG: parse body\r\n");
    request->body = cJSON_DetachItemFromObject(JSON, "body");

    if (request->cmd == OPCUA_CLIENT_CONNECT){
        if (parse_connect_request( request->body ) != 0){ goto error; }
    } else if( request->cmd == OPCUA_CLIENT_READ ){
        if (parse_read_request( request->body ) != 0) { goto error; }
    }else if( request->cmd == OPCUA_CLIENT_WRITE ){
        if (parse_write_request( request->body ) != 0){ goto error; }
    }else if( request->cmd == OPCUA_CLIENT_SUBSCRIBE ){
        if (parse_subscribe_request( request->body ) != 0){ goto error; }
    }else if( request->cmd == OPCUA_CLIENT_UPDATE_SUBSCRIPTIONS ){
        if (parse_update_subscriptions_request( request->body ) != 0){ goto error; }
    }else if( request->cmd == OPCUA_CLIENT_BROWSE_ENDPOINTS ){
        if (parse_browse_endpoints_request( request->body ) != 0){ goto error; }
    }else if( request->cmd == OPCUA_CLIENT_BROWSE_FOLDER ){
        if (parse_browse_folder_request( request->body ) != 0){ goto error; }
    } else {
        LOGERROR("ERROR: invalid command type %d\r\n",request->cmd);
        goto error;
    }
    if (request->body == NULL){
        LOGERROR("ERROR: unable to parse request body\r\n");
        goto error;
    }

    cJSON_Delete( JSON );

    return 0;

error:

    cJSON_Delete( JSON );
    return -1;
}

// Clean the memory used for the request structure
void purge_request( OPCUA_CLIENT_REQUEST* request ){
    cJSON_Delete( request->body );
}

// Build the response
char* create_response( OPCUA_CLIENT_REQUEST *request, cJSON *responseBody ){
    cJSON *response = cJSON_CreateObject();
    char *responseString = NULL;

    // Inherit command type from the request
    char *cmd = cmd2string( request->cmd );
    if (cmd == NULL){
        goto error;
    }
    if ( cJSON_AddStringToObject(response, "cmd", cmd) == NULL) {
        goto error;
    }

    // Inherit transaction id from the request
    if ( cJSON_AddNumberToObject(response, "tid", request->tid) == NULL) {
        goto error;
    } 

    // Add the response body
    if ( !cJSON_AddItemToObject(response, "reply", responseBody) ) {
        goto error;
    }
    responseString = cJSON_PrintUnformatted( response );
    if (responseString == NULL)
    {
        LOGERROR("ERROR: unable to print response.\r\n");
        goto error;
    }

    cJSON_Delete( response );
    return responseString;

error:
    cJSON_Delete( responseBody );
    cJSON_Delete( response );
    return "{\"type\":\"error\",\"text\":\"unable to construct the response\"}";
}

//------------------Internal helpers--------------------------------------------
OPCUA_CLIENT_CMD string2cmd(char *cmd){
    if ( strcmp(cmd, "connect") == 0 ){
        return OPCUA_CLIENT_CONNECT;
    }else if( strcmp(cmd, "read") == 0){
        return OPCUA_CLIENT_READ;
    }else if( strcmp(cmd, "write") == 0){
        return OPCUA_CLIENT_WRITE;
    }else if( strcmp(cmd, "subscribe") == 0){
        return OPCUA_CLIENT_SUBSCRIBE;
    }else if( strcmp(cmd, "update_subscriptions") == 0){
        return OPCUA_CLIENT_UPDATE_SUBSCRIPTIONS;
    }else if( strcmp(cmd, "browse_endpoints") == 0){
        return OPCUA_CLIENT_BROWSE_ENDPOINTS;
    }else if( strcmp(cmd, "browse_folder") == 0){
        return OPCUA_CLIENT_BROWSE_FOLDER;
    }else{
        return -1; 
    }
}

char* cmd2string(OPCUA_CLIENT_CMD cmd){
    if ( cmd == OPCUA_CLIENT_CONNECT ){
        return "connect";
    }else if( cmd == OPCUA_CLIENT_READ){
        return "read";
    }else if( cmd == OPCUA_CLIENT_WRITE){
        return "write";
    }else if( cmd == OPCUA_CLIENT_SUBSCRIBE ){
        return "subscribe";
    }else if( cmd == OPCUA_CLIENT_UPDATE_SUBSCRIPTIONS ){
        return "update_subscriptions";
    }else if( cmd == OPCUA_CLIENT_BROWSE_ENDPOINTS ){
        return "browse_endpoints";
    }else if( cmd == OPCUA_CLIENT_BROWSE_FOLDER ){
        return "browse_folder";
    }else{
        return NULL; 
    }
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
int parse_connect_request( cJSON *request ){
    cJSON *host = NULL;
    cJSON *endpoint = NULL;
    cJSON *port = NULL;
    cJSON *login = NULL;
    cJSON *certificate = NULL;
    cJSON *privateKey = NULL;
    cJSON *password = NULL;
    char *connectionString = NULL;

    if ( !cJSON_IsObject(request) ) {
        LOGERROR("ERROR: invalid connection parameters\r\n");
        goto error;
    }

    // Parse host
    host = cJSON_DetachItemFromObject(request, "host");
    if (!cJSON_IsString(host) || (host->valuestring == NULL)){
        LOGERROR("ERROR: host is not defined\r\n");
        goto error; 
    }

    // Parse port
    port = cJSON_DetachItemFromObject(request, "port");
    if (!cJSON_IsNumber(port)){
        LOGERROR("ERROR: port is not defined\r\n");
        goto error; 
    }

    // Parse host
    endpoint = cJSON_DetachItemFromObject(request, "endpoint");
    if (!cJSON_IsString(host) || (host->valuestring == NULL)){
        endpoint = NULL;
    }

    // Parse login (optional)
    certificate = cJSON_GetObjectItemCaseSensitive(request, "certificate");
    if (cJSON_IsString(certificate) && (certificate->valuestring != NULL)){
        // It is a secure connection, the key must be provided
        privateKey = cJSON_GetObjectItemCaseSensitive(request, "private_key");
        if (!cJSON_IsString(privateKey) || (privateKey->valuestring == NULL)){
            LOGERROR("ERROR: key is not defined\r\n");
            goto error; 
        }
    }else{
        certificate = NULL;
    }
    // Parse login (optional)
    login = cJSON_GetObjectItemCaseSensitive(request, "login");
    if (cJSON_IsString(login) && (login->valuestring != NULL)){

        // If the login is provided then the password is required
        password = cJSON_GetObjectItemCaseSensitive(request, "password");
        if (!cJSON_IsString(password) || (password->valuestring == NULL)){
            LOGERROR("ERROR: password is not defined\r\n");
            goto error; 
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
    LOGDEBUG("DEBUG: url %s\r\n",connectionString);

    if (cJSON_AddStringToObject(request, "url", connectionString) == NULL) {
        LOGERROR("ERROR: unable to add the url to the request object\r\n");
        goto error; 
    }
    free(connectionString);
    cJSON_Delete( host );
    cJSON_Delete( port );
    cJSON_Delete( endpoint );

    LOGDEBUG("DEBUG: return parsed connect request\r\n");
    
    return 0;

error:
    if (connectionString != NULL){
        free(connectionString);
    }
    cJSON_Delete( host );
    cJSON_Delete( port );
    cJSON_Delete( endpoint );
    return -1;
}

int parse_read_request( cJSON *request ){
    cJSON *item = NULL;
    cJSON *name = NULL;

    if ( !cJSON_IsArray(request) ) {
        LOGERROR("ERROR: invalid read parameters\r\n");
        goto error;
    }

    // Copy request items
    cJSON_ArrayForEach(item, request) {
        if (!cJSON_IsArray(item)){
            LOGERROR("ERROR: invalid item to read\r\n");
            goto error;
        }
        cJSON_ArrayForEach(name, item) {
            if (!cJSON_IsString(name) || (name->valuestring == NULL)) {
                LOGERROR("ERROR: invalid item to read\r\n");
                goto error;
            }
        }
    }
    return 0;

error:
    return -1;
}

int parse_write_request( cJSON *request ){
    cJSON *tag = NULL;
    cJSON *value = NULL;
    cJSON *item = NULL;
    cJSON *name = NULL;

    if ( !cJSON_IsArray(request) ) {
        LOGERROR("ERROR: invalid write parameters\r\n");
        goto error;
    }

    cJSON_ArrayForEach(item, request) {
        if (!cJSON_IsArray(item)) {
            LOGERROR("ERROR: invalid item to write\r\n");
            goto error;
        }

        tag = cJSON_GetArrayItem(item, 0);
        cJSON_ArrayForEach(name, tag) {
            if (!cJSON_IsString(name) || (name->valuestring == NULL)) {
                LOGERROR("ERROR: invalid item to write\r\n");
                goto error;
            }
        }

        value = cJSON_GetArrayItem(item, 1);

        if (cJSON_IsString(value) && (value->valuestring != NULL)) {
            // OK
        }else if(cJSON_IsNumber(value)){
            // OK
        }else if( cJSON_IsBool(value) ){
            if (cJSON_IsFalse(value)){
                value = cJSON_CreateNumber(0);
            }else{
                value = cJSON_CreateNumber(1);
            }
            if (value == NULL){
                LOGERROR("ERROR: unable alocate cJSON object for value\r\n");
                goto error;
            }
            // cJSON purges the previous ite itself
            if ( !cJSON_ReplaceItemInArray(item, 1, value) ){
                LOGERROR("ERROR: unable to coerce the value\r\n");
                goto error;
            }
        }else{
            LOGERROR("ERROR: invalid value to write\r\n");
            goto error;
        }
    }

    LOGDEBUG("DEBUG: return parsed write request\r\n");
    return 0;

error:
    return -1;
}

int parse_subscribe_request( cJSON *request ){
    cJSON *item = NULL;
    cJSON *name = NULL;

    if ( !cJSON_IsArray(request) ) {
        LOGERROR("ERROR: invalid subscribe parameters\r\n");
        goto error;
    }

    // Check request items
    cJSON_ArrayForEach(item, request) {
        if (!cJSON_IsArray(item)){
            LOGERROR("ERROR: invalid item to subscribe\r\n");
            goto error;
        }
        cJSON_ArrayForEach(name, item) {
            if (!cJSON_IsString(name) || (name->valuestring == NULL)) {
                LOGERROR("ERROR: invalid item to subscribe\r\n");
                goto error;
            }
        }
    }
    return 0;

error:
    return -1;
}

int parse_update_subscriptions_request( cJSON *request ){
    return 0;
}

int parse_browse_endpoints_request( cJSON *request ){
    cJSON *host = NULL;
    cJSON *port = NULL;

    if ( !cJSON_IsObject(request) ) {
        LOGERROR("ERROR: invalid parameters\r\n");
        goto error;
    }

    // Parse host
    host = cJSON_GetObjectItemCaseSensitive(request, "host");
    if (!cJSON_IsString(host) || (host->valuestring == NULL)){
        LOGERROR("ERROR: host is not defined\r\n");
        goto error; 
    }

    // Parse port
    port = cJSON_GetObjectItemCaseSensitive(request, "port");
    if (!cJSON_IsNumber(port)){
        LOGERROR("ERROR: port is not defined\r\n");
        goto error; 
    }
    
    return 0;

error:
    return -1;
}

int parse_browse_folder_request( cJSON *request ){
    cJSON *item = NULL;

    if ( !cJSON_IsArray(request) ) {
        LOGERROR("ERROR: invalid folder parameter\r\n");
        goto error;
    }

    // Copy request items
    cJSON_ArrayForEach(item, request) {
        if (!cJSON_IsString(item) || (item->valuestring == NULL)) {
            LOGERROR("ERROR: invalid item to browse\r\n");
            goto error;
        }
    }
    return 0;

error:
    return -1;
}

