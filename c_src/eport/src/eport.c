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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <try_catch.h>
#include "eport.h"

int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);
int read_cmd(byte **buf);
int write_cmd(byte *buf, int len);

void eport_loop(eport_request_handler callback){

    byte *request;
    byte *response;
    int len = 0;
    cJSON *requestJSON = NULL;
    cJSON *responseJSON = NULL;

    //-----------the loop--------------------------
    while ( 1 ){
        request = NULL;
        response = NULL;
        // wait for a request
        len = read_cmd( &request );
        if (len == EOF) {
            if (request != NULL){
                free(request);
            }
            break;
        }

        LOGTRACE("DEBUG: message received: %s\r\n",(char *)request);

        char *error = NULL;
        TRY(
            // parse JSON
            requestJSON = parse_request( request, &error );

            // Handle the request with the callback
            responseJSON = callback( requestJSON, &error );

        )CATCH( exception,
            LOGERROR("undefined exception %d", exception);
        );

        if (responseJSON != NULL){
                
        }else{
            // there must be some error
            if (error == NULL){
                responseJSON = on_error( requestJSON, "undefined error" );
            }else{
                responseJSON = on_error( requestJSON, error );
            }
        }

        cJSON_Delete( requestJSON ); requestJSON = NULL;
        cJSON_Delete( responseJSON ); responseJSON = NULL;
        free( request );
        free( response );
        if (error != NULL){
            free( error );
        }

        // request is not needed any longer, free its memory
        free(request);

        // analyze the response
        if ( response == NULL){
            response = (byte *)"programming error: NULL response";
        }
        len = strlen( (char *)response );
        LOGDEBUG("DEBUG: reply with %s\r\n",response);
        write_cmd( response, len );
        free( response );
    }

    LOGDEBUG("EXIT port");
}

//----------Read/Write helpers----------------------------------------
int read_exact(byte *buf, int len) {
    int i, got=0;
    do {
        if ((i = read(IN_DESC, buf+got, len-got)) <= 0){
            return EOF;
        }
        got += i;
    } while (got<len);

  return(len);
}

int write_exact(byte *buf, int len) {
    int i, wrote = 0;
    do {
        if ((i = write(OUT_DESC, buf+wrote, len-wrote)) <= 0)
        return (i);
        wrote += i;
    } while (wrote<len);

    return (len);
}

int read_cmd(byte **buf) {
    byte lbuf[HEADER_LENGTH];
    int len;
    int i;

    // Step 1. Read the length of the message
    if (read_exact(lbuf, HEADER_LENGTH) != HEADER_LENGTH) return(-1);

    // Convert the length buffer to the integer
    len = 0;
    for (i = 0; i < HEADER_LENGTH; i++){
        len = len | (lbuf[i] << (8 * (HEADER_LENGTH - i -1)) );
    }

    // Step 2. Read the message itself
    *buf = malloc(len); // dynamically allocate the memory for the message
    if (*buf == NULL){
        return -1;
    }
    return read_exact(*buf, len);
}

int write_cmd(byte *buf, int len){
    byte lbuf[HEADER_LENGTH];
    int i;

    // Convert the length from integer to the big-endian
    for (i = 0; i < HEADER_LENGTH; i++){
        lbuf[i] = 0xff & ( len >> (8 * (HEADER_LENGTH - i -1)) ) ;
    }

    // Send the response
    write_exact(lbuf, HEADER_LENGTH);
    return write_exact(buf, len);
}

cJSON * parse_request( const char *request, char *error ){

    // Parse the request to JSON structure
    const cJSON *request = cJSON_Parse( message );
    if (JSON == NULL){
        const char *error_ptr = cJSON_GetErrorPtr();
        if (error_ptr != NULL) {
            RAISE("ERROR: invalid JSON before: %s\r\n", error_ptr);
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