/*----------------------------------------------------------------
* Copyright (c) 2022 Faceplate
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
//----------------------------------------
#include <eport_c.h>
//----------------------------------------
#include <openssl/x509v3.h>
#include <openssl/bn.h>
#include <openssl/asn1.h>
#include <openssl/x509.h>
#include <openssl/x509_vfy.h>
#include <openssl/pem.h>
#include <openssl/bio.h>
//----------------------------------------
#include "opcua_server_loop.h"
#include "opcua_server_nodes.h"

//-----------------------------------------------------
//  eport_c API
//-----------------------------------------------------
static cJSON* opcua_server_start(cJSON* args, char **error){

    *error = start( args );
    if(*error) return NULL;

    // the server has started
    return cJSON_CreateString("ok");
}

//-------------------------------------------------------------
//  The entry point for creating and chnging values for nodes.
//  It waits args in the next format:
//  {
//      "path": "some/path/to/my/variable",
//      "type": "UInt32",
//      "value": 56
//  }
//  If the node is already added to the server the mtheod 
//  changes its value. If the path doesn't exist yet the node
//  is created and the value is set.
//-------------------------------------------------------------
static cJSON* opcua_server_write_item(cJSON* args, char **error){
    LOGTRACE("write item");

    if (!is_started()){
        *error = "server not started";
        goto on_error;
    }

    //-----------validate the arguments-----------------------
    cJSON *path = cJSON_GetObjectItemCaseSensitive(args, "path");
    if (!cJSON_IsString(path) || (path->valuestring == NULL)){
        *error = "path is not defined";
        goto on_error; 
    }

    cJSON *type = cJSON_GetObjectItemCaseSensitive(args, "type");
    if (!cJSON_IsString(type) || (type->valuestring == NULL)){
        *error = "type is not defined";
        goto on_error; 
    }

    cJSON *value = cJSON_GetObjectItemCaseSensitive(args, "value");
    if (value == NULL){
        *error = "value is not defined";
        goto on_error; 
    }

    UA_NodeId *nodeId = lookup_node( path->valuestring );
    if (!nodeId){
        LOGINFO("create new node %s",path->valuestring);
        *error = create_node( path->valuestring, &nodeId );
        if (*error) goto on_error;
    }

    // write the value
    *error = write_value(nodeId, type->valuestring, value );
    if (*error) goto on_error;

    return cJSON_CreateString("ok");

on_error:
    return NULL;
}

static cJSON* opcua_server_write_items(cJSON* args, char **error){
    LOGTRACE("write items");

    cJSON *response = NULL;
    cJSON *item = NULL;
    cJSON *result = NULL;

    if (!is_started()){
        *error = "server not started";
        goto on_error;
    }

    //-----------validate the arguments-----------------------
    if ( !cJSON_IsArray(args) ) {
        *error = "invalid write arguments";
        goto on_error;
    }

    response = cJSON_CreateArray();

    cJSON_ArrayForEach(item, args) {
        result = opcua_server_write_item( item, error );
        if (result == NULL){
            char _error[strlen(*error) + strlen("error: ") +1 ];
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

static cJSON* opcua_server_read_item(cJSON* args, char **error){
    LOGTRACE("read item");

    if (!is_started()){
        *error = "server not started";
        goto on_error;
    }

    //-----------validate the arguments-----------------------
    if (!cJSON_IsString(args) || (args->valuestring == NULL)){
        *error = "item is not defined";
        goto on_error; 
    }

    UA_NodeId *nodeId = lookup_node( args->valuestring );
    if (!nodeId){
        *error = "invalid node";
        goto on_error;
    }

    cJSON *value = NULL;
    *error = read_value( nodeId, &value );
    if (*error) goto on_error;

    return value;

on_error:
    return NULL;
}

static cJSON* opcua_server_read_items(cJSON* args, char **error){
    LOGTRACE("read items");
    cJSON *response = NULL;
    cJSON *item = NULL;
    cJSON *result = NULL;

    if (!is_started()){
        *error = "server not started";
        goto on_error;
    }

    //-----------validate the arguments-----------------------
    if ( !cJSON_IsArray(args) ) {
        *error = "invalid arguments";
        goto on_error;
    }

    response = cJSON_CreateArray();
    cJSON_ArrayForEach(item, args) {
        result = opcua_server_read_item( item, error );
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

//-----------------------------------------------------
//  eport_c request routing
//-----------------------------------------------------
static cJSON* on_request( char *method, cJSON *args, char **error ){
    
    cJSON *response = NULL;
    // Handle the request
    LOGTRACE("handle the request %s", method);

    if( strcmp(method, "server_start") == 0){
        response = opcua_server_start( args, error );
    }else if( strcmp(method, "write_items") == 0){
        response = opcua_server_write_items( args, error );
    }else if( strcmp(method, "write_item") == 0){
        response = opcua_server_write_item( args, error );
    }else if( strcmp(method, "read_items") == 0){
        response = opcua_server_read_items( args, error );
    }else if( strcmp(method, "read_item") == 0){
        response = opcua_server_read_item( args, error );
    }else{
        *error = "invalid method";
    }

    return response;
}

//------------------------catch signals------------------------------------------------
static void stopHandler(int sig) {
    LOGINFO("received ctrl-c");
    stop();
}

//------------------------THE ENTRY POINT------------------------------------------------
int main(int argc, char *argv[]) {

    signal(SIGINT, stopHandler);
    signal(SIGTERM, stopHandler);

    OpenSSL_add_all_algorithms();
    ERR_load_BIO_strings();

    LOGINFO("enter eport_loop");
    eport_loop( &on_request );

    stop();

    return EXIT_SUCCESS;
}
