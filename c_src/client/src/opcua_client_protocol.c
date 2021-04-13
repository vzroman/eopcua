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
#include "opcua_client_protocol.h"

int parse_connect_request( cJSON *body );
int parse_read_request( cJSON *body );
int parse_write_request( cJSON *body );
int parse_subscribe_request( cJSON *body );
int parse_browse_endpoints_request( cJSON *body );
int parse_browse_folder_request( cJSON *body );

OPCUA_CLIENT_CMD string2cmd(char *cmd);
char* cmd2string(OPCUA_CLIENT_CMD cmd);


OPCUA_CLIENT_REQUEST* parse_request( const char *message ){

    const cJSON *cmd = NULL;
    const cJSON *tid = NULL;
    OPCUA_CLIENT_REQUEST *request = malloc( sizeof(OPCUA_CLIENT_REQUEST) );
    request->body = NULL;

    // Parse the request to JSON structure
    fprintf(stdout,"DEBUG: parse JSON\r\n");
    cJSON *JSON = cJSON_Parse( message );
    if (JSON == NULL){
        const char *error_ptr = cJSON_GetErrorPtr();
        if (error_ptr != NULL) {
            fprintf(stdout,"error on parsing JSON before: %s\r\n", error_ptr);
        }else{
            fprintf(stdout,"undefined error on parsing JSON\r\n");
        }
        goto error;
    }

    // Parse the type of the request
    fprintf(stdout,"DEBUG: parse cmd\r\n");
    cmd = cJSON_GetObjectItemCaseSensitive(JSON, "cmd");
    if (cJSON_IsString(cmd) && (cmd->valuestring != NULL)){
        request->cmd = string2cmd( cmd->valuestring );
        if (request->cmd == -1){
            fprintf(stdout,"invalid command type: %s\r\n", cmd->valuestring);
            goto error;
        }
    } else {
        fprintf(stdout,"invalid request format: command type is not defined\r\n");
        goto error;
    }

    // Parse transaction ID
    fprintf(stdout,"DEBUG: parse tid\r\n");
    tid = cJSON_GetObjectItemCaseSensitive(JSON, "tid");
    if ( cJSON_IsNumber(tid)) {
        request->tid = tid->valuedouble;
    } else{
        fprintf(stdout,"invalid request format: transaction id not defined\r\n");
        goto error;
    }
    
    // Parse body
    fprintf(stdout,"DEBUG: parse body\r\n");
    request->body = cJSON_GetObjectItemCaseSensitive(JSON, "body");
    if (request->cmd == OPCUA_CLIENT_CONNECT){
        fprintf(stdout,"DEBUG: parse connect body\r\n");
        if (parse_connect_request( request->body ) != 0){ goto error; }
    } else if( request->cmd == OPCUA_CLIENT_READ ){
        fprintf(stdout,"DEBUG: parse read body\r\n");
        if (parse_read_request( request->body ) != 0) { goto error; }
    }else if( request->cmd == OPCUA_CLIENT_WRITE ){
        fprintf(stdout,"DEBUG: parse write body\r\n");
        if (parse_write_request( request->body ) != 0){ goto error; }
    }else if( request->cmd == OPCUA_CLIENT_SUBSCRIBE ){
        fprintf(stdout,"DEBUG: parse subscribe body\r\n");
        if (parse_subscribe_request( request->body ) != 0){ goto error; }
    }else if( request->cmd == OPCUA_CLIENT_BROWSE_ENDPOINTS ){
        fprintf(stdout,"DEBUG: parse browse endpoints body\r\n");
        if (parse_browse_endpoints_request( request->body ) != 0){ goto error; }
    }else if( request->cmd == OPCUA_CLIENT_BROWSE_FOLDER ){
        fprintf(stdout,"DEBUG: parse browse folder body\r\n");
        if (parse_browse_folder_request( request->body ) != 0){ goto error; }
    } else {
        fprintf(stdout,"invalid command type %d\r\n",request->cmd);
        goto error;
    }
    if (request->body == NULL){
        fprintf(stdout,"unable to parse request body\r\n");
        goto error;
    }

    return request;

error:
    purge_request( request );
    cJSON_Delete( JSON );
    return NULL;
}

// Clean the memory used for the request structure
void purge_request( OPCUA_CLIENT_REQUEST* request ){
    if( request != NULL ){
        cJSON_Delete( request->body );
        free( request );
    }
}

// Build the response
char* create_response( OPCUA_CLIENT_REQUEST *request, cJSON *responseBody ){
    cJSON *response = cJSON_CreateObject();
    char *responseString = NULL;
    if( request == NULL ){
        goto error;
    }

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
        fprintf(stderr, "Failed to print response.\r\n");
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
//         "login":"user1",
//         "password":"secret"
//     }
int parse_connect_request( cJSON *request ){
    cJSON *host = NULL;
    cJSON *endpoint = NULL;
    cJSON *port = NULL;
    cJSON *login = NULL;
    cJSON *password = NULL;
    char *connectionString = NULL;

    if ( !cJSON_IsObject(request) ) {
        fprintf(stdout,"ERROR: invalid connection parameters\r\n");
        goto error;
    }

    // Parse host
    host = cJSON_DetachItemFromObject(request, "host");
    if (!cJSON_IsString(host) || (host->valuestring == NULL)){
        fprintf(stdout,"ERROR: host is not defined\r\n");
        goto error; 
    }

    // Parse port
    port = cJSON_DetachItemFromObject(request, "port");
    if (!cJSON_IsNumber(port)){
        fprintf(stdout,"ERROR: port is not defined\r\n");
        goto error; 
    }

    // Parse host
    endpoint = cJSON_DetachItemFromObject(request, "endpoint");
    if (!cJSON_IsString(host) || (host->valuestring == NULL)){
        endpoint = NULL;
    }

    fprintf(stdout,"DEBUG: parsing login/passowrd\r\n");

    // Parse login (optional)
    login = cJSON_GetObjectItemCaseSensitive(request, "login");
    if (cJSON_IsString(login) && (login->valuestring != NULL)){
        // If the login is provided then the password is required
        password = cJSON_GetObjectItemCaseSensitive(request, "password");
        if (!cJSON_IsString(password) || (password->valuestring == NULL)){
            fprintf(stdout,"ERROR: password is not defined\r\n");
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
    fprintf(stdout,"DEBUG: url %s\r\n",connectionString);

    if (cJSON_AddStringToObject(request, "url", connectionString) == NULL) {
        fprintf(stdout,"unable to add the url to the request object\r\n");
        goto error; 
    }
    free(connectionString);
    cJSON_Delete( host );
    cJSON_Delete( port );
    cJSON_Delete( endpoint );

    fprintf(stdout,"DEBUG: return parsed connect request\r\n");
    
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

    if ( !cJSON_IsArray(request) ) {
        fprintf(stdout,"ERROR: invalid read parameters\r\n");
        goto error;
    }

    // Copy request items
    cJSON_ArrayForEach(item, request) {
        if (!cJSON_IsString(item) || (item->valuestring == NULL)) {
            fprintf(stdout,"ERROR: invalid item to read\r\n");
            goto error;
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

    if ( !cJSON_IsObject(request) ) {
        fprintf(stdout,"ERROR: invalid write parameters\r\n");
        goto error;
    }

    // ------------Tag---------------------------------
    tag = cJSON_GetObjectItemCaseSensitive(request, "tag");
    if (!cJSON_IsArray(tag)) {
        fprintf(stdout,"ERROR: invalid tag\r\n");
        goto error;
    }
    cJSON_ArrayForEach(item, tag) {
        if (!cJSON_IsString(item) || (item->valuestring == NULL)) {
            fprintf(stdout,"ERROR: invalid item to write\r\n");
            goto error;
        }
    }

    // ---------value---------------------------------------
    value = cJSON_GetObjectItemCaseSensitive(request, "value");
    fprintf(stdout,"DEBUG: add value to request\r\n");
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
            fprintf(stdout,"ERROR: unable alocate cJSON object for value\r\n");
            goto error;
        }
        // cJSON purges the previous ite itself
        if ( !cJSON_ReplaceItemInObjectCaseSensitive(request,"value",value) ){
            fprintf(stdout,"ERROR: unable to coerce the value\r\n");
            goto error;
        }
    }else{
        fprintf(stdout,"ERROR: invalid value to write\r\n");
        goto error;
    }

    fprintf(stdout,"DEBUG: return parsed write request\r\n");
    return 0;

error:
    return -1;
}

int parse_subscribe_request( cJSON *request ){
    cJSON *item = NULL;

    if ( !cJSON_IsArray(request) ) {
        fprintf(stdout,"ERROR: invalid subscribe parameter\r\n");
        goto error;
    }

    cJSON_ArrayForEach(item, request) {
        if (!cJSON_IsString(item) || (item->valuestring == NULL)) {
            fprintf(stdout,"ERROR: invalid item to subscribe\r\n");
            goto error;
        }
    }
    return 0;

error:
    return -1;
}

int parse_browse_endpoints_request( cJSON *request ){
    cJSON *host = NULL;
    cJSON *port = NULL;

    if ( !cJSON_IsObject(request) ) {
        fprintf(stdout,"ERROR: invalid parameters\r\n");
        goto error;
    }

    // Parse host
    host = cJSON_GetObjectItemCaseSensitive(request, "host");
    if (!cJSON_IsString(host) || (host->valuestring == NULL)){
        fprintf(stdout,"ERROR: host is not defined\r\n");
        goto error; 
    }

    // Parse port
    port = cJSON_GetObjectItemCaseSensitive(request, "port");
    if (!cJSON_IsNumber(port)){
        fprintf(stdout,"ERROR: port is not defined\r\n");
        goto error; 
    }
    
    return 0;

error:
    return -1;
}

int parse_browse_folder_request( cJSON *request ){
    cJSON *item = NULL;

    if ( !cJSON_IsArray(request) ) {
        fprintf(stdout,"ERROR: invalid folder parameter\r\n");
        goto error;
    }

    // Copy request items
    cJSON_ArrayForEach(item, request) {
        if (!cJSON_IsString(item) || (item->valuestring == NULL)) {
            fprintf(stdout,"ERROR: invalid item to browse\r\n");
            goto error;
        }
    }
    return 0;

error:
    return -1;
}

