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
  int nodeClass;
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

static char *insertRefArray(RefArray *a, char *name, UA_NodeId *nodeId, int nodeClass) {
    if (a->used >= a->size) {
        a->size += a->step;
        a->array = realloc(a->array, a->size * sizeof(RefArrayEntry));
        if (!a->array) return "out of memory";
    }

    a->array[a->used].nodeId = nodeId;
    a->array[a->used].name = strdup(name);
    a->array[a->used].nodeClass = nodeClass;

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

static char *getNodePath(UA_NodeId *folder, char *name, char **path){
    UA_NodeId root = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
    if (UA_NodeId_equal(folder, &root)){
        // This is the 'Objects' folder
        *path = strdup( name );
        return NULL;
    }

    char *context = lookup_nodeId2path_cache( folder );
    if (!context) {
        return "unable to find path by nodeId";
    }

    size_t length = strlen(context) + 1 + strlen(name);
    *path = malloc(length + 1);
    if (!*path) return "out of memory";

    sprintf(*path,"%s/%s",context, name);
    (*path)[length] = '\0';

    return NULL;
}

//---------------------------------------------------------------------------
//  Build Browse Cache
//---------------------------------------------------------------------------
static char *build_browse_cache_inner(UA_Client *client, RefArray *folders, size_t maxNodesPerBrowse){

    char *error = NULL;

    // Build the request
    UA_BrowseRequest request;
    UA_BrowseResponse response;

    UA_BrowseRequest_init(&request);
    UA_BrowseResponse_init(&response);

    // Found subfolders
    RefArray subfolders; subfolders.used = 0;
    error = initRefArray(&subfolders, 500);
    if (error) goto on_clear;

    // Set the limit
    size_t limit = maxNodesPerBrowse;
    if (limit == 0 || folders->used < limit) limit = folders->used;

    //----------------Folders cycle----------------------------------
    for (size_t shift = 0; shift < folders->used; shift += limit){
        // TODO. The limit can be defined at the server, we have to handle it
        request.requestedMaxReferencesPerNode = 0;

        // Configure the request
        size_t size = folders->used - shift;
        if (size > limit) size = limit;

        request.nodesToBrowse = UA_Array_new(size, &UA_TYPES[UA_TYPES_BROWSEDESCRIPTION]);
        request.nodesToBrowseSize = size;

        for(size_t i = 0; i < size; ++i) {
            // Take only folders and variables
            request.nodesToBrowse[i].nodeClassMask = UA_NODECLASS_OBJECT | UA_NODECLASS_VARIABLE;
            request.nodesToBrowse[i].includeSubtypes = true;
            request.nodesToBrowse[i].referenceTypeId = UA_NODEID_NUMERIC(0, UA_NS0ID_HIERARCHICALREFERENCES);
            UA_NodeId_copy(folders->array[i + shift].nodeId, &request.nodesToBrowse[i].nodeId);
            request.nodesToBrowse[i].resultMask = UA_BROWSERESULTMASK_BROWSENAME | UA_BROWSERESULTMASK_NODECLASS;
        }

        // Trigger the request
        response = UA_Client_Service_browse(client, request);
        if (response.responseHeader.serviceResult != UA_STATUSCODE_GOOD){
            error = (char*)UA_StatusCode_name( response.responseHeader.serviceResult );
            goto on_clear;
        }

        //---------------results cycle------------------------------
        for(size_t i = 0; i < response.resultsSize; ++i) {
            for(size_t j = 0; j < response.results[i].referencesSize; ++j) {

                UA_ReferenceDescription *ref = &response.results[i].references[j];
                
                // Should we shows objects inside variables? 
                if (ref->nodeClass == UA_NODECLASS_OBJECT && folders->array[i + shift].nodeClass == UA_NODECLASS_VARIABLE)
                    continue;

                char *name = (char *)ref->browseName.name.data;
                char *path = NULL;
                error = getNodePath(folders->array[i + shift].nodeId, name, &path);
                if (error) goto on_clear;
                
                // Check if the node already in
                UA_NodeId *exists = lookup_path2nodeId_cache( path );
                if(exists) continue;

                UA_NodeId *nodeIdCopy = UA_NodeId_new();
                UA_StatusCode sc = UA_NodeId_copy(&ref->nodeId.nodeId, nodeIdCopy);
                if (sc != UA_STATUSCODE_GOOD){
                    error = (char*)UA_StatusCode_name( sc );
                    goto on_clear;
                }
                
                error = add_cache(path, nodeIdCopy, ref->nodeClass);
                if (error) goto on_clear; 

                // Add children recursively
                error = insertRefArray(&subfolders, name, nodeIdCopy, ref->nodeClass);
                if (error) goto on_clear;
            }
        }   
        //---------------results cycle end------------------------------

        // Prepare fro the next step
        UA_BrowseRequest_clear(&request);
        UA_BrowseResponse_clear(&response);
        UA_BrowseRequest_init(&request);
        UA_BrowseResponse_init(&response);
    }
    //---------------folders cycle end------------------------------

    if (subfolders.used){
        error = build_browse_cache_inner(client, &subfolders, maxNodesPerBrowse );
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
char *build_browse_cache(UA_Client *client, size_t maxNodesPerBrowse){
    char *error;

    // Init folders array with 'Objects' folder
    RefArray folders;
    error = initRefArray(&folders, 1);
    if(error) goto on_clear;
    UA_NodeId root = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
    error = insertRefArray(&folders, "", &root, UA_NODECLASS_OBJECT);
    if(error) goto on_clear;

    error = build_browse_cache_inner(client, &folders, maxNodesPerBrowse);
    if (error) goto on_clear;

on_clear:
    freeRefArray( &folders );
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

