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
#include <open62541/server.h>
#include <uthash.h>

#include "utilities.h"
#include "opcua_server_loop.h"
#include "opcua_server_nodes.h"

typedef struct {
  char *path;
  UA_NodeId *nodeId;
  UT_hash_handle hh;
} path2nodeId_cache;

path2nodeId_cache *__path2nodeId_cache = NULL;

static char *add_node(char *path, UA_NodeId nodeId, UA_NodeId **outNodeId){
    char *error = NULL;

    path2nodeId_cache *path2nodeId = NULL;

    // Build path2nodeId index
    path2nodeId = (path2nodeId_cache *)malloc( sizeof(path2nodeId_cache) );
    if (!path2nodeId){
        error = "out of memory";
        goto on_error;
    }

    // Allocate a new nodeId for cache
    path2nodeId->nodeId = UA_NodeId_new();
    if(!path2nodeId->nodeId){
        error = "unable to allocate nodeId";
        goto on_error;
    }
    UA_NodeId_copy(&nodeId, path2nodeId->nodeId);
    path2nodeId->path = strdup(path);

    HASH_ADD_STR(__path2nodeId_cache, path, path2nodeId);

    *outNodeId = path2nodeId->nodeId;
    
    return NULL;

on_error:
  if (path2nodeId) free(path2nodeId);
  return error;
}

UA_NodeId *lookup_node(char *path){
    path2nodeId_cache *path2nodeId = NULL;
    HASH_FIND_STR(__path2nodeId_cache, path, path2nodeId);
    if (path2nodeId == NULL){
        return NULL;
    }else{
       return path2nodeId->nodeId;
    }
}

void purge_nodes(){
    // Purge path2nodeId index
    path2nodeId_cache *path2nodeId;
    for (path2nodeId= __path2nodeId_cache; path2nodeId != NULL; path2nodeId = path2nodeId->hh.next) {
        HASH_DEL(__path2nodeId_cache, path2nodeId);
        free( path2nodeId->path );
        // MEMORY LEAK! We have to free nodeId but it causes 
        //      malloc_consolidate(): invalid chunk size
        //UA_NodeId_delete( path2NodeId->nodeId );
        free( path2nodeId );
    }
}

static char *ensure_path(char **path, UA_NodeId folder, char* context, UA_NodeId **outNodeId){

    char *name = *path;

    int l = strlen(name);
    if(context){
        l += strlen(context) + 1;
    }
    char _path[l+1];
    if(context){
        sprintf(_path,"%s/%s",context,name);
    }else{
        sprintf(_path,"%s",name);
    }

    *outNodeId = lookup_node( _path );
    if(!*outNodeId) {
        UA_NodeId _outNodeId;
        char *error = add_folder(folder, name, &_outNodeId);
        if(error) return error;

        error = add_node(_path, _outNodeId, outNodeId);
        if(error) return error;
    }

    if (*(path + 1)){
        return ensure_path( path + 1, **outNodeId, _path, outNodeId);
    }

    return NULL;
}

char *create_node(char *path, UA_NodeId **outNodeId){
    char *error = NULL;

    char *name = NULL; 
    char **tokens = NULL;

    UA_NodeId *folder;
    tokens = str_split( path, '/');
    if (tokens){
        // Extract the name from the path
        int depth = 0; while (*(tokens + depth)) depth++;
        name = *(tokens + depth -1);
        *(tokens + depth -1) = NULL;

        // Create a folder if not exists
        error = ensure_path(tokens, UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER), NULL, &folder);
        if (error) {
            free( name );
            goto on_clear;
        }
    }else{
        name = strdup( path );
    }

    UA_NodeId _outNodeId;
    error = add_variable(*folder, name, &_outNodeId);
    if (error) goto on_clear;

    error = add_node(path, _outNodeId, outNodeId);

on_clear:
    if (name) free( name );
    if (tokens) str_split_destroy( tokens );
    return error;
}







