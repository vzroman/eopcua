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

cJSON* parse_connect_request( cJSON *body );
cJSON* parse_read_request( cJSON *body );
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
        request->body = parse_connect_request( request->body );
    } else if( request->cmd == OPCUA_CLIENT_READ ){
        fprintf(stdout,"DEBUG: parse read body\r\n");
        request->body = parse_read_request( request->body );
    } else {
        fprintf(stdout,"invalid command type %d\r\n",request->cmd);
        goto error;
    }
    if (request->body == NULL){
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

    return responseString;

error:
    cJSON_Delete( response );
    return "{\"type\":\"error\",\"text\":\"unable to construct the response\"}";
}

//------------------Internal helpers--------------------------------------------
OPCUA_CLIENT_CMD string2cmd(char *cmd){
    if ( strcmp(cmd, "connect") == 0 ){
        return OPCUA_CLIENT_CONNECT;
    }else if( strcmp(cmd, "close") == 0){
        return OPCUA_CLIENT_CLOSE;
    }else if( strcmp(cmd, "read") == 0){
        return OPCUA_CLIENT_READ;
    }else if( strcmp(cmd, "write") == 0){
        return OPCUA_CLIENT_WRITE;
    }else{
        return -1; 
    }
}

char* cmd2string(OPCUA_CLIENT_CMD cmd){
    if ( cmd == OPCUA_CLIENT_CONNECT ){
        return "connect";
    }else if( cmd == OPCUA_CLIENT_CLOSE ){
        return "close";
    }else if( cmd == OPCUA_CLIENT_READ){
        return "read";
    }else if( cmd == OPCUA_CLIENT_WRITE){
        return "write";
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
cJSON* parse_connect_request( cJSON *inBody ){
    cJSON *request = NULL;
    cJSON *host = NULL;
    cJSON *endpoint = NULL;
    cJSON *port = NULL;
    cJSON *login = NULL;
    cJSON *password = NULL;
    char *connectonString = NULL;

    if ( !cJSON_IsObject(inBody) ) {
        fprintf(stdout,"invalid connection parameters\r\n");
        goto error;
    }

    // Parse host
    host = cJSON_GetObjectItemCaseSensitive(inBody, "host");
    if (!cJSON_IsString(host) || (host->valuestring == NULL)){
        fprintf(stdout,"host is not defined\r\n");
        goto error; 
    }

    // Parse port
    port = cJSON_GetObjectItemCaseSensitive(inBody, "port");
    if (!cJSON_IsNumber(port)){
        fprintf(stdout,"port is not defined\r\n");
        goto error; 
    }

    // Parse host
    endpoint = cJSON_GetObjectItemCaseSensitive(inBody, "endpoint");
    if (!cJSON_IsString(host) || (host->valuestring == NULL)){
        endpoint = NULL;
    }

    fprintf(stdout,"DEBUG: parsing login/passowrd\r\n");

    // Parse login (optional)
    login = cJSON_GetObjectItemCaseSensitive(inBody, "login");
    if (cJSON_IsString(login) && (login->valuestring != NULL)){
        // If the login is provided then the password is required
        password = cJSON_GetObjectItemCaseSensitive(inBody, "password");
        if (!cJSON_IsString(password) || (password->valuestring == NULL)){
            fprintf(stdout,"password is not defined\r\n");
            goto error; 
        }
    }else{
        login = NULL;
    }

    // Building the connection string (6 in tail is :<port> as port max string length is 5)
    char *prefix = "opc.tcp://";
    int urlLen = strlen(prefix) + strlen(host->valuestring) + 6; // :65535 is max
    if (endpoint != NULL){
        urlLen += strlen( endpoint->valuestring );
    }
    fprintf(stdout,"DEBUG: urlLen %d\r\n",urlLen);

    connectonString = malloc( urlLen );
    if (endpoint != NULL){
        sprintf(connectonString, "%s%s:%d/%s", prefix, host->valuestring, (int)port->valuedouble, endpoint->valuestring);
    }else{
        sprintf(connectonString, "%s%s:%d", prefix, host->valuestring, (int)port->valuedouble);
    }
    fprintf(stdout,"DEBUG: url %s\r\n",connectonString);


    // Build the request structure
    request = cJSON_CreateObject();
    if (request == NULL){
        fprintf(stdout,"unable to alocate the request object\r\n");
        goto error; 
    }
    if (cJSON_AddStringToObject(request, "url", connectonString) == NULL) {
        fprintf(stdout,"unable to add the url to the request object\r\n");
        goto error; 
    }
    free(connectonString);

    if (login != NULL && password != NULL){
        if (cJSON_AddStringToObject(request, "login", login->valuestring) == NULL) {
            fprintf(stdout,"unable to add the login to the request object\r\n");
            goto error; 
        }
         if (cJSON_AddStringToObject(request, "password", password->valuestring) == NULL) {
            fprintf(stdout,"unable to add the password to the request object\r\n");
            goto error; 
        }
    }

    fprintf(stdout,"DEBUG: return parsed connect request\r\n");
    
    return request;

error:
    if (connectonString != NULL){
        free(connectonString);
    }
    cJSON_Delete( request );
    cJSON_Delete( host );
    cJSON_Delete( port );
    cJSON_Delete( login );
    cJSON_Delete( password );
    return NULL;
}

cJSON* parse_read_request( cJSON *inBody ){
    cJSON *request = NULL;
    cJSON *item = NULL;
    cJSON *iter = NULL;

    if ( !cJSON_IsArray(inBody) ) {
        fprintf(stdout,"invalid read parameters\r\n");
        goto error;
    }

    // Copy request items
    request = cJSON_CreateArray();
    cJSON_ArrayForEach(item, inBody) {
        if (!cJSON_IsString(item) || (item->valuestring == NULL)) {
            fprintf(stdout,"invalid item to read\r\n");
            goto error;
        }
        iter = cJSON_CreateString( item->valuestring );
        if (item == NULL){
            fprintf(stdout,"error on creating an item %s for read collection\r\n", item->valuestring );
            goto error;
        }
        if ( !cJSON_AddItemToArray(request, iter) ) {
            fprintf(stdout,"unable to add read item to array %s\r\n", item->valuestring);
            goto error;
        }

    }
    
    return request;

error:
    cJSON_Delete( request );
    cJSON_Delete( item );
    return NULL;
}