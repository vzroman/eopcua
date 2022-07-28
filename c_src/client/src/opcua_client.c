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
//----------------------------------------
#include <eport_c.h>
//----------------------------------------
#include <openssl/x509v3.h>
//----------------------------------------
#include "utilities.h"
#include "opcua_client_browse.h"
#include "opcua_client_loop.h"

//-----------------------------------------------------
//  eport_c API
//-----------------------------------------------------
static cJSON* opcua_client_browse_servers(cJSON* args, char **error){

    cJSON *response = NULL;

    if ( !cJSON_IsObject(args) ) {
        *error = "invalid parameters";
        goto on_clear;
    }

    cJSON *host = cJSON_GetObjectItemCaseSensitive(args, "host");
    if (!cJSON_IsString(host) || (host->valuestring == NULL)){
        *error = "host is not defined";
        goto on_clear; 
    }

    cJSON *port = cJSON_GetObjectItemCaseSensitive(args, "port");
    if (!cJSON_IsNumber(port)){
        *error = "port is not defined";
        goto on_clear; 
    }

    char **urls = NULL;
    *error = browse_servers(host->valuestring, port->valueint, &urls);
    if (*error) goto on_clear;

    // Build the response
    response = cJSON_CreateArray();
    if(!response){
        *error = "unable to allocate CJSON object for response";
        goto on_clear;
    }

    for (int i = 0; *(urls + i); i++){
        cJSON *URL = cJSON_CreateString( *(urls + i) );
        if (!URL){
            *error = "unable to allocate CJSON object for URL";
            goto on_clear;
        } 
        if (!cJSON_AddItemToArray(response,URL)){
            *error = "unable to add endpoint to the array";
            goto on_clear;
        }
    }

on_clear:
    if (urls){
        for (int i = 0; *(urls + i); i++){
            free(*(urls + i));
        }
        free(urls);
    }

    if (!*error) return response;

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
//         "update_cycle":200,
//         "max_nodes_per_browse":1000
//     }
static cJSON* opcua_client_connect(cJSON* args, char **error){
    if ( is_started() ){
        *error = "already connected";
        goto on_error;
    }

    if ( !cJSON_IsObject(args) ) {
        *error = "invalid parameters";
        goto on_error;
    }

    //-----------validate the arguments-----------------------
    cJSON *url = cJSON_GetObjectItemCaseSensitive(args, "url");
    if (!cJSON_IsString(url) || (url->valuestring == NULL)){
        *error = "url is not defined";
        goto on_error; 
    }
    char *_url = url->valuestring;

    uint _update_cycle = 0;
    cJSON *update_cycle = cJSON_GetObjectItemCaseSensitive(args, "update_cycle");
    if (cJSON_IsNumber(update_cycle)){
        _update_cycle = (uint)update_cycle->valueint; 
    }

    size_t _max_nodes_per_browse = 0;
    cJSON *max_nodes_per_browse = cJSON_GetObjectItemCaseSensitive(args, "max_nodes_per_browse");
    if (cJSON_IsNumber(max_nodes_per_browse)){
        _max_nodes_per_browse = (size_t)max_nodes_per_browse->valueint; 
    }

    char *_certificate = NULL;
    char *_privateKey = NULL;
    cJSON *certificate = cJSON_GetObjectItemCaseSensitive(args, "certificate");
    if (cJSON_IsString(certificate) && (certificate->valuestring != NULL)){
        // It is a secure connection, the key must be provided
        cJSON *privateKey = cJSON_GetObjectItemCaseSensitive(args, "private_key");
        if (!cJSON_IsString(privateKey) || (privateKey->valuestring == NULL)){
            *error = "key is not defined";
            goto on_error; 
        }
        _certificate = certificate->valuestring;
        _privateKey = privateKey->valuestring;
    }

    char *_login = NULL;
    char *_password = NULL;
    cJSON *login = cJSON_GetObjectItemCaseSensitive(args, "login");
    if (cJSON_IsString(login) && (login->valuestring != NULL)){

        // If the login is provided then the password is required
        cJSON *password = cJSON_GetObjectItemCaseSensitive(args, "password");
        if (!cJSON_IsString(password) || (password->valuestring == NULL)){
            *error = "password is not defined";
            goto on_error; 
        }

        _login = login->valuestring;
        _password = password->valuestring;
    }

    //--------------Connecting procedure------------------------------
    *error = start(
        _url,
        _certificate,
        _privateKey,
        _login,
        _password,
        _update_cycle,
        _max_nodes_per_browse
    );
    if (*error) goto on_error;

    return cJSON_CreateString("ok");

on_error:
    return NULL;
}

static cJSON* opcua_client_read_item(cJSON* args, char **error){
    LOGTRACE("read item");

    if (!is_started()){
        *error = "no connection";
        goto on_error;
    }

    //-----------validate the arguments-----------------------
    if (!cJSON_IsString(args) || (args->valuestring == NULL)){
        *error = "path is not defined";
        goto on_error; 
    }

    cJSON *value = NULL;
    *error = read_value( args->valuestring, &value );
    if (*error) goto on_error;

    return value;

on_error:
    return NULL;
}

static cJSON* opcua_client_read_items(cJSON* args, char **error){
    LOGTRACE("read items");
    cJSON *response = NULL;
    cJSON *item = NULL;

    UA_NodeId **nodeId = NULL;
    UA_DataValue *values = NULL;
    size_t valid = 0;

    if (!is_started()){
        *error = "no connection";
        goto on_clear;
    }

    //-----------validate the arguments-----------------------
    if ( !cJSON_IsArray(args) ) {
        *error = "invalid read arguments";
        goto on_clear;
    }

    size_t size = cJSON_GetArraySize( args );

    nodeId = malloc( size * sizeof(UA_NodeId *));
    if(!nodeId){
        *error = "out of memory";
        goto on_clear;
    }

    response = cJSON_CreateObject();
    if (!response){
        *error = "unable to create response object";
        goto on_clear;
    }

    cJSON_ArrayForEach(item, args) {
        UA_NodeId *n = lookup_path2nodeId_cache( item->valuestring );
        if (!n){
            if (!cJSON_AddStringToObject(response, item->valuestring, "error: invalid node")){
                *error = "unable to add item to response";
                goto on_clear;
            }
        }else{
            //LOGINFO("DEBUG: add to valid %s",item->valuestring);
            nodeId[valid++] = n;
        }
    }

    *error = read_values(valid, nodeId, &values);
    if (*error) goto on_clear;

    for(size_t i=0; i<valid; i++){

        char *path = lookup_nodeId2path_cache(nodeId[i]);
        //LOGINFO("DEBUG: result for %s",path);
        
        if (values[i].status != UA_STATUSCODE_GOOD){
            char *e = (char*)UA_StatusCode_name( values[i].status );
            char _err[strlen(e) + 8]; // error: 
            sprintf(_err,"error: %s",e);
            if (!cJSON_AddStringToObject(response, path, _err)){
                *error = "unable to add item to response";
                goto on_clear;
            }
        }else{
            cJSON *value = ua2json( values[i].value.type, values[i].value.data );
            if(!value){
                if (!cJSON_AddStringToObject(response, path, "error: invalid value")){
                    *error = "unable to add item to response";
                    goto on_clear;
                }
            }else{
                if (!cJSON_AddItemToObject(response, path, value)){
                    *error = "unable to add item to response";
                    goto on_clear;
                }
            }
        }
    }

on_clear:
    if(nodeId) free(nodeId);
    if(values) UA_Array_delete(values, valid, &UA_TYPES[UA_TYPES_DATAVALUE]);

    if(!*error) return response;

    cJSON_Delete( response );
    return NULL;
}

static cJSON* opcua_client_write_item(cJSON* args, char **error){

    if (!is_started()){
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

    *error = write_value(tag->valuestring, value);
    if (*error) goto on_error;
    
    return cJSON_CreateString("ok");

on_error:
    return NULL;
}

static cJSON* opcua_client_write_items(cJSON* args, char **error){
    LOGTRACE("write items");
    cJSON *response = cJSON_CreateArray();
    cJSON *item = NULL;
    cJSON *result = NULL;

    if (!is_started()){
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

static cJSON* opcua_client_search(cJSON* args, char **error){
    cJSON *response = NULL;
    opcua_item *items = NULL;

    if (!is_started()){
        *error = "no connection";
        goto on_error;
    }

    if (!cJSON_IsString(args) || (args->valuestring == NULL)){
        *error = "undefined search string";
        goto on_error;
    }

    char *search = args->valuestring;;

    response = cJSON_CreateObject();
    if (!response){
        *error = "unable to create result set";
        goto on_error;
    }

    items = get_all_cache_items();
    for (int i = 0; items[i].nodeId; i++){
        if (strstr(items[i].path, search)){
            if (!cJSON_AddNumberToObject(response,items[i].path, items[i].nodeClass)){
                *error = "unable to add an item to the result";
                goto on_error;
            }
        }
    }
    free(items);

    return response;

on_error:
    if (items) free(items);
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
    }else if (strcmp(method, "search") == 0){
        response = opcua_client_search( args, error );
    } else{
        *error = "invalid method";
    }

    return response;
}

//------------------------THE ENTRY POINT------------------------------------------------
int main(int argc, char *argv[]) {

    OpenSSL_add_all_algorithms();
    ERR_load_BIO_strings();

    LOGINFO("enter eport_loop");
    eport_loop( &on_request );

    return EXIT_SUCCESS;
}