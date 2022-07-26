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

#include <uthash.h>
#include <open62541/types_generated_handling.h>

#include "opcua_client_browse_cache.h"
//-----------------------------------------------------
//  Cache
//-----------------------------------------------------
typedef struct {
  char *path;
  UA_NodeId *nodeId;
  UT_hash_handle hh;
} opcua_client_path2nodeId_cache;

typedef struct {
  UA_NodeId *nodeId;
  char *path;
  UT_hash_handle hh;
} opcua_client_nodeId2path_cache;


opcua_client_path2nodeId_cache *__path2nodeId_cache;
opcua_client_nodeId2path_cache *__nodeId2path_cache;

char *add_cache(char *path, UA_NodeId *nodeId){
    char *error = NULL;

    opcua_client_path2nodeId_cache *path2NodeId = NULL;
    opcua_client_nodeId2path_cache *nodeId2path = NULL;

    // Build path2nodeId index
    path2NodeId = (opcua_client_path2nodeId_cache *)malloc( sizeof(opcua_client_path2nodeId_cache) );
    if (!path2NodeId){
        error = "unable to allocate the memory for a cache entry";
        goto on_error;
    }
    path2NodeId->nodeId = nodeId;
    path2NodeId->path = path;

    // Build nodeId2path index
    nodeId2path = (opcua_client_nodeId2path_cache *)malloc( sizeof(opcua_client_nodeId2path_cache) );
    if (!nodeId2path){
        error = "unable to allocate the memory for a cache entry";
        goto on_error;
    }
    nodeId2path->nodeId = nodeId;
    nodeId2path->path = path;

    HASH_ADD_STR(__path2nodeId_cache, path, path2NodeId);
    HASH_ADD_PTR(__nodeId2path_cache, nodeId, nodeId2path);

    return NULL;

on_error:
  if (path2NodeId) free(path2NodeId);
  if (nodeId2path) free(nodeId2path);
  return error;
}

UA_NodeId *lookup_path2nodeId_cache(char *path){
    opcua_client_path2nodeId_cache *path2NodeId = NULL;
    HASH_FIND_STR(__path2nodeId_cache, path, path2NodeId);
    if (path2NodeId == NULL){
        return NULL;
    }else{
       return path2NodeId->nodeId;
    }
}

char *lookup_nodeId2path_cache(UA_NodeId *nodeId){
    opcua_client_nodeId2path_cache *nodeId2path = NULL;

    HASH_FIND_PTR(__nodeId2path_cache, &nodeId, nodeId2path);
    if (nodeId2path == NULL){
        return NULL;
    }else{
       return nodeId2path->path;
    }
}

char **get_all_cache_items(){

    size_t size = HASH_CNT(hh, __path2nodeId_cache);
    char **items = (char **)malloc( sizeof(char*) * (size + 1) );
    size_t i = 0;

    opcua_client_path2nodeId_cache *path2NodeId;
    for (path2NodeId= __path2nodeId_cache; path2NodeId != NULL; path2NodeId = path2NodeId->hh.next) {
        items[i++] = path2NodeId->path;
    }
    items[i] = NULL;
    return items;
}

void purge_cache(){

    // Purge path2nodeId index
    opcua_client_path2nodeId_cache *path2NodeId;
    for (path2NodeId= __path2nodeId_cache; path2NodeId != NULL; path2NodeId = path2NodeId->hh.next) {
        HASH_DEL(__path2nodeId_cache, path2NodeId);
        free( path2NodeId->path );
        // MEMORY LEAK! We have to free nodeId but it causes 
        //      malloc_consolidate(): invalid chunk size
        //UA_NodeId_delete( path2NodeId->nodeId );
        free( path2NodeId );
    }

    // Purge nodeId2path index
    opcua_client_nodeId2path_cache *nodeId2path;
    for (nodeId2path= __nodeId2path_cache; nodeId2path != NULL; nodeId2path = nodeId2path->hh.next) {
        HASH_DEL(__nodeId2path_cache, nodeId2path);
        free( nodeId2path );
    }
}

