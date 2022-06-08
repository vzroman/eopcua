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

int parse_start_request( cJSON *body );

OPCUA_SERVER_CMD string2cmd(char *cmd);
char* cmd2string(OPCUA_SERVER_CMD cmd);


int parse_request( const char *message, OPCUA_SERVER_REQUEST* request ){

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

    if (request->cmd == OPCUA_SERVER_START){
        if (parse_start_request( request->body ) != 0){ goto error; }
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
void purge_request( OPCUA_SERVER_REQUEST* request ){
    cJSON_Delete( request->body );
}

// Build the response
char* create_response( OPCUA_SERVER_REQUEST *request, cJSON *responseBody ){
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
OPCUA_SERVER_CMD string2cmd(char *cmd){
    if ( strcmp(cmd, "start_server") == 0 ){
        return OPCUA_SERVER_START;
    }else{
        return -1; 
    }
}

char* cmd2string(OPCUA_SERVER_CMD cmd){
    if ( cmd == OPCUA_SERVER_START ){
        return "start_server";
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
int parse_start_request( cJSON *request ){
    
    LOGDEBUG("DEBUG: return parsed start request\r\n");
    
    return 0;
}


