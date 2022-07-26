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
#include <open62541/types.h>
#include <open62541/types_generated.h>
#include <open62541/types_generated_handling.h>

#include <eport_c_log.h>

#include "opcua_client_browse_cache.h"
#include "opcua_client_browse.h"

//-----------------------------------------------------
//  Dynamic Reference Array
//-----------------------------------------------------
typedef struct{
  UA_NodeId *nodeId;
  char *name;
  UA_NodeClass nodeClass;
} RefArrayEntry;

typedef struct{
  RefArrayEntry *array;
  size_t step;
  size_t used;
  size_t size;
} RefArray;

static char *initRefArray(RefArray *a, size_t initialSize) {
    a->array = malloc(initialSize * sizeof(RefArrayEntry));
    if (!a->array) return "out of memory";
    a->step = initialSize;
    a->size = initialSize;
    a->used = 0;
    return NULL;
}

static char *insertRefArray(RefArray *a, char *name, UA_NodeId *nodeId, UA_NodeClass nodeClass) {
    if (a->used >= a->size) {
        a->size += a->step;
        a->array = realloc(a->array, a->size * sizeof(RefArrayEntry));
        if (!a->array) return "out of memory";
    }

    a->array[a->used].nodeId = nodeId;
    a->array[a->used].nodeClass = nodeClass;
    a->array[a->used].name = strdup( name );

    a->used++;

    return NULL;
}

static void freeRefArray(RefArray *a){
    for (int i = 0; i < a->used; i++){
        free(a->array[i].name);
    }
    free(a->array);
    a->array = NULL;
    a->size = 0;
    a->used = 0;
}

//---------------------------------------------------------------------------
//  Build Browse Cache
//---------------------------------------------------------------------------
static char *browse_folder(UA_Client *client, UA_NodeId folder, RefArray *result){

    char *error = NULL;

    // Build the request
    UA_BrowseRequest request;
    UA_BrowseRequest_init(&request);

    UA_BrowseResponse response;
    UA_BrowseResponse_init(&response);

    // The batches by 500 items
    error = initRefArray(result, 500);
    if (error) goto on_clear;

    // Configure the request
    request.requestedMaxReferencesPerNode = 0;    
    request.nodesToBrowse = UA_BrowseDescription_new();
    request.nodesToBrowseSize = 1;
    request.nodesToBrowse[0].nodeId = folder; 
    request.nodesToBrowse[0].resultMask = UA_BROWSERESULTMASK_BROWSENAME | UA_BROWSERESULTMASK_NODECLASS;

    response = UA_Client_Service_browse(client, request);

    if (response.responseHeader.serviceResult != UA_STATUSCODE_GOOD){
        error = (char*)UA_StatusCode_name( response.responseHeader.serviceResult );
        goto on_clear;
    }

    for(size_t i = 0; i < response.resultsSize; ++i) {
        for(size_t j = 0; j < response.results[i].referencesSize; ++j) {
            UA_ReferenceDescription *ref = &response.results[i].references[j];

            // Take only folders and variables
            if(ref->nodeClass != UA_NODECLASS_OBJECT && ref->nodeClass != UA_NODECLASS_VARIABLE)
                continue;
            
            // Add a copy of the reference
            UA_NodeId *nodeIdCopy = UA_NodeId_new();
            UA_StatusCode sc = UA_NodeId_copy(&ref->nodeId.nodeId, nodeIdCopy);
            if (sc != UA_STATUSCODE_GOOD){
                error = (char*)UA_StatusCode_name( sc );
                goto on_clear;
            }

            error = insertRefArray(result, (char *)ref->browseName.name.data, nodeIdCopy, ref->nodeClass);
            if (error) goto on_clear;
        }
    }

on_clear:
    // Clear the main request/response
    UA_BrowseRequest_clear(&request);
    UA_BrowseResponse_clear(&response);

    if (!error) return NULL;
    
    freeRefArray( result );
    return error;
}

static char *build_browse_cache_inner(UA_Client *client, RefArray *folders){

    char *error = NULL;

    RefArray subfolders; subfolders.used = 0;
    error = initRefArray(&subfolders, 500);
    if (error) goto on_clear;

    // Build the request
    UA_BrowseRequest request;
    UA_BrowseRequest_init(&request);

    UA_BrowseResponse response;
    UA_BrowseResponse_init(&response);

    // Configure the request
    request.requestedMaxReferencesPerNode = 0;    

    request.nodesToBrowse = UA_Array_new(folders->used, &UA_TYPES[UA_TYPES_BROWSEDESCRIPTION]);
    request.nodesToBrowseSize = folders->used;

    for(size_t i = 0; i < folders->used; ++i) {
        request.nodesToBrowse[i].nodeId = *folders->array[i].nodeId; 
        request.nodesToBrowse[i].resultMask = UA_BROWSERESULTMASK_BROWSENAME | UA_BROWSERESULTMASK_NODECLASS;
    }

    response = UA_Client_Service_browse(client, request);
    if (response.responseHeader.serviceResult != UA_STATUSCODE_GOOD){
        error = (char*)UA_StatusCode_name( response.responseHeader.serviceResult );
        goto on_clear;
    }

    for(size_t i = 0; i < response.resultsSize; ++i) {
        for(size_t j = 0; j < response.results[i].referencesSize; ++j) {

            UA_ReferenceDescription *ref = &response.results[i].references[j];

            // Take only folders and variables
            if(ref->nodeClass != UA_NODECLASS_OBJECT && ref->nodeClass != UA_NODECLASS_VARIABLE)
                continue;

            char *name = (char *)ref->browseName.name.data;
            char *context = lookup_nodeId2path_cache( folders->array[i].nodeId );
            if (!context) {
                error = "unable to find path by nodeId";
                goto on_clear;
            }

            char path[strlen(context) + 1 + strlen(name)+1];
            sprintf(path,"%s/%s",context, name);

            UA_NodeId *nodeIdCopy = UA_NodeId_new();
            UA_StatusCode sc = UA_NodeId_copy(&ref->nodeId.nodeId, nodeIdCopy);
            if (sc != UA_STATUSCODE_GOOD){
                error = (char*)UA_StatusCode_name( sc );
                goto on_clear;
            }
            error = add_cache(path, nodeIdCopy );
            if (error) goto on_clear; 

            // Add children recursively
            if(ref->nodeClass == UA_NODECLASS_OBJECT){
                error = insertRefArray(&subfolders, (char *)ref->browseName.name.data, nodeIdCopy, ref->nodeClass);
                if (error) goto on_clear;
            }
        }
    }

    if (subfolders.used){
        error = build_browse_cache_inner(client, &subfolders );
        if (error) goto on_clear;
    }

on_clear:
    // Clear the main request/response
    UA_BrowseRequest_clear(&request);
    UA_BrowseResponse_clear(&response);
    freeRefArray( &subfolders );
    return error;
}

//---------------------------------------------------------------------------
//  API
//---------------------------------------------------------------------------
char *build_browse_cache(UA_Client *client){
    char *error;
    UA_NodeId root = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);

    RefArray items; items.used = 0;
    error = browse_folder(client, root, &items);
    if(error) goto on_clear;

    // Found in the root subfolders
    RefArray subfolders; subfolders.used = 0;
    error = initRefArray(&subfolders, 500);
    if (error) goto on_clear;

    for(size_t i = 0; i < items.used; ++i) {

        char * name = items.array[i].name;

        error = add_cache(name, items.array[i].nodeId );
        if (error) goto on_clear; 

        // Add children recursively
        if(items.array[i].nodeClass == UA_NODECLASS_OBJECT){
            error = insertRefArray(&subfolders, items.array[i].name, items.array[i].nodeId, items.array[i].nodeClass );
            if (error) goto on_clear;
        }
    }

    if (subfolders.used){
        error = build_browse_cache_inner(client, &subfolders );
        if (error) goto on_clear;
    }

on_clear:
    freeRefArray( &items );
    freeRefArray( &subfolders );
    return error;
}

// Lookup nodeId by its path
char *path2nodeId( char *path, UA_NodeId *nodeId ){
    // Cached version
    UA_NodeId *cached = lookup_path2nodeId_cache( path );
    if (!cached){
        return "invalid node";
    }
    *nodeId = *cached;
    return NULL;
}

