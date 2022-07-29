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

static char *write_item(cJSON *item){
    LOGTRACE("write item %s",item->string);

    if(!cJSON_IsObject(item)) return "invalid arguments";

    cJSON *type = cJSON_GetObjectItemCaseSensitive(item, "type");
    if (!cJSON_IsString(type) || (!type->valuestring)) return "type not provided";

    cJSON *value = cJSON_GetObjectItemCaseSensitive(item, "value");
    if (value == NULL) return "value is not provided";

    UA_NodeId *nodeId = lookup_node( item->string );
    if (!nodeId){
        LOGINFO("create new node %s",item->string);
        char *error = create_node( item->string, &nodeId );
        if (error) return error;
    }

    // write the value
    return write_value(nodeId, type->valuestring, value );
}

static cJSON* opcua_server_write_items(cJSON* args, char **error){
    LOGTRACE("write items");

    if (!is_started()){
        *error = "server not started";
        return NULL;
    }

    //-----------validate the arguments-----------------------
    if ( !cJSON_IsObject(args) ) {
        *error = "invalid write arguments";
        return NULL;
    }

    cJSON *response = cJSON_CreateObject();

    cJSON *item = NULL; char *err = NULL;
    cJSON_ArrayForEach(item, args) {
        err = write_item( item );
        if (err){
            cJSON_AddStringToObject( response, item->string, err );
        }else{
            cJSON_AddStringToObject( response, item->string, "ok" );
        }
    }

    return response;
}

static char* read_item(char* item, cJSON **value){
    LOGTRACE("read item %s",item);

    UA_NodeId *nodeId = lookup_node( item );
    if (!nodeId){
        LOGINFO("create new node %s",item);
        char *error = create_node( item, &nodeId );
        if (error) return error;
    }

    return read_value( nodeId, value );
}

static cJSON* opcua_server_read_items(cJSON* args, char **error){
    LOGTRACE("read items");

    if (!is_started()){
        *error = "server not started";
        return NULL;
    }

    //-----------validate the arguments-----------------------
    if ( !cJSON_IsArray(args) ) {
        *error = "invalid arguments";
        return NULL;
    }

    cJSON *response = cJSON_CreateObject();
    char *err = NULL;

    cJSON *item = NULL;  cJSON *result = NULL;
    cJSON_ArrayForEach(item, args) {
        err = read_item( item->valuestring, &result );
        if (err){
            cJSON_AddStringToObject(response, item->valuestring, err);
        }else{
            cJSON_AddItemToObject(response, item->valuestring, result);
        }
    }

    return response;
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
    }else if( strcmp(method, "read_items") == 0){
        response = opcua_server_read_items( args, error );
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
