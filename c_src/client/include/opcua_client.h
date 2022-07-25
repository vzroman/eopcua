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
#include <open62541/types.h>

//-----------------------------------------------------
//  Subscription indexes
//-----------------------------------------------------
typedef struct {
  char *path;
  int id;
  UT_hash_handle hh;
} opcua_client_binding;

typedef struct {
  int id;
  UA_Variant *value;
  UA_StatusCode status;
  UA_DataType *type;
  UA_NodeId nodeId;
  UT_hash_handle hh;
} opcua_client_subscription; 

opcua_client_binding *__bindings_index = NULL;
opcua_client_subscription *__subscriptions_index = NULL;

static opcua_client_subscription *add_subscription(
    char *path, 
    int monId, 
    UA_NodeId *nodeId, 
    UA_Variant *nodeValue, 
    UA_StatusCode nodeStatus, 
    UA_DataType *nodeType, 
    char **error
){
    // Add the binding to the collection
    opcua_client_binding *b = NULL;
    opcua_client_subscription *s = NULL;
    b = (opcua_client_binding *)malloc(sizeof(opcua_client_binding));
    if (b == NULL){
        *error = "unable to allocate the memory for a new binding index";
        goto on_error;
    }
    b->path = strdup( path );
    b->id = monId;

    s = (opcua_client_subscription *)malloc(sizeof(opcua_client_subscription));
    if (s == NULL){
        *error = "unable to allocate the memory for new binding";
        goto on_error;
    }
    UA_StatusCode sc = UA_NodeId_copy(nodeId, &s->nodeId);
    if (sc != UA_STATUSCODE_GOOD){
        *error = (char*)UA_StatusCode_name( sc );
        goto on_error;
    }
    //s->nodeId = nodeId;
    s->id = monId;
    s->value = nodeValue;
    s->status = nodeStatus;
    s->type = nodeType;

    HASH_ADD_STR(__bindings_index, path, b);
    HASH_ADD_INT(__subscriptions_index, id, s);

    return s;
on_error:
  if(b) free(b);
  if(s) free(s);
  return NULL;
}

static opcua_client_subscription *find_subscription(char *path, char **error){
    *error = NULL;
    // Lookup the binding in the collection
    opcua_client_binding *b = NULL;
    opcua_client_subscription *s = NULL;
    HASH_FIND_STR(__bindings_index, path, b);
    if (b == NULL){
        return NULL;
    }else{
        HASH_FIND_INT(__subscriptions_index, &b->id, s);
        if (s == NULL) *error = "invalid subscription index";
    }
    return s;
}

static char *update_subscription(int monId, UA_DataValue *value){
    char *error = NULL;

    opcua_client_subscription *s = NULL;
    HASH_FIND_INT(__subscriptions_index, &monId, s);
    if (s != NULL){
        // Update the value
        UA_StatusCode sc = UA_Variant_copy( &value->value, s->value );
        if (UA_Variant_copy( &value->value, s->value ) != UA_STATUSCODE_GOOD){
            error = "unable to copy value on subscription update";
        }
        if ( value->status != UA_STATUSCODE_GOOD){
            s->status = value->status;
        }else{
            s->status = sc;
        }
    }else{
        error = "unable to update subscription";
    }
    return error;
}

static void purge_subscriptions(){
    opcua_client_binding *b;
    for (b = __bindings_index; b != NULL; b = b->hh.next) {
        HASH_DEL(__bindings_index, b);
        free( b->path );
        free( b );
    }
    __bindings_index = NULL;

    opcua_client_subscription *s;
    for (s = __subscriptions_index; s != NULL; s = s->hh.next) {
        HASH_DEL(__subscriptions_index, s);
        UA_Variant_delete(s->value);
        UA_NodeId_delete( &s->nodeId );
        free( s );
    }
    __subscriptions_index = NULL;
}

//-----------------------------------------------------
//  Cache
//-----------------------------------------------------
typedef struct {
  char *path;
  UA_NodeId nodeId;
  UT_hash_handle hh;
} opcua_client_path2nodeId_cache;

typedef struct {
  UA_NodeId nodeId;
  char *path;
  UT_hash_handle hh;
} opcua_client_nodeId2path_cache;

typedef void (*CacheIteratorCallback)(UA_NodeId nodeId, char *path);

opcua_client_path2nodeId_cache *__path2nodeId_cache;
opcua_client_nodeId2path_cache *__nodeId2path_cache;

static char *add_cache(char *path, UA_NodeId *nodeId){
    char *error = NULL;

    opcua_client_path2nodeId_cache *path2NodeId = NULL;
    opcua_client_nodeId2path_cache *nodeId2path = NULL;

    // Build path2nodeId index
    path2NodeId = (opcua_client_path2nodeId_cache *)malloc( sizeof(opcua_client_path2nodeId_cache) );
    if (!path2NodeId){
        error = "unable to allocate the memory for a cache entry";
        goto on_error;
    }
    path2NodeId->path = strdup(path);
    UA_StatusCode sc = UA_NodeId_copy(nodeId, &path2NodeId->nodeId);
    if (sc != UA_STATUSCODE_GOOD){
        error = (char*)UA_StatusCode_name( sc );
        goto on_error;
    }

    // Build nodeId2path index
    nodeId2path = (opcua_client_nodeId2path_cache *)malloc( sizeof(opcua_client_nodeId2path_cache) );
    if (!nodeId2path){
        error = "unable to allocate the memory for a cache entry";
        goto on_error;
    }
    nodeId2path->path = strdup(path);
    sc = UA_NodeId_copy(nodeId, &nodeId2path->nodeId);
    if (sc != UA_STATUSCODE_GOOD){
        error = (char*)UA_StatusCode_name( sc );
        goto on_error;
    }

    HASH_ADD_STR(__path2nodeId_cache, path, path2NodeId);
    HASH_ADD(hh, __nodeId2path_cache, nodeId, sizeof(UA_NodeId), nodeId2path);

    return NULL;

on_error:
  if (path2NodeId) free(path2NodeId);
  if (nodeId2path) free(nodeId2path);
  return error;
}

static UA_NodeId *lookup_path2nodeId_cache(char *path){
    opcua_client_path2nodeId_cache *path2NodeId = NULL;
    HASH_FIND_STR(__path2nodeId_cache, path, path2NodeId);
    if (path2NodeId == NULL){
        return NULL;
    }else{
       return &path2NodeId->nodeId;
    }
}

static char *lookup_nodeId2path_cache(UA_NodeId nodeId){
    opcua_client_nodeId2path_cache *nodeId2path = NULL;

    HASH_FIND(hh, __nodeId2path_cache, &nodeId, sizeof(UA_NodeId), nodeId2path);

    if (nodeId2path == NULL){
        return NULL;
    }else{
       return nodeId2path->path;
    }
}

static char **get_all_cache_items(){

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

static void purge_cache(){

    // Purge path2nodeId index
    opcua_client_path2nodeId_cache *path2NodeId;
    for (path2NodeId= __path2nodeId_cache; path2NodeId != NULL; path2NodeId = path2NodeId->hh.next) {
        HASH_DEL(__path2nodeId_cache, path2NodeId);
        free( path2NodeId->path );
        free( path2NodeId );
    }

    // Purge nodeId2path index
    opcua_client_nodeId2path_cache *nodeId2path, *tmp;
    HASH_ITER(hh, __nodeId2path_cache, nodeId2path, tmp) {
      HASH_DEL(__nodeId2path_cache, nodeId2path);
      free( nodeId2path->path );
      free( nodeId2path );
    }
}


//-----------------------------------------------------
//  Dynamic Reference Array
//-----------------------------------------------------
typedef struct{
  UA_ReferenceDescription *array;
  size_t step;
  size_t used;
  size_t size;
} RefArray;

static char *initRefArray(RefArray *a, size_t initialSize) {
    a->array = malloc(initialSize * sizeof(UA_ReferenceDescription));
    if (!a->array) return "out of memory";
    a->step = initialSize;
    a->size = initialSize;
    a->used = 0;
    return NULL;
}

static char *insertRefArray(RefArray *a, UA_ReferenceDescription element) {
    if (a->used >= a->size) {
        a->size += a->step;
        a->array = realloc(a->array, a->size * sizeof(UA_ReferenceDescription));
        if (!a->array) return "out of memory";
    }

    UA_StatusCode sc = UA_ReferenceDescription_copy(&element, &a->array[a->used++]);
    if (sc != UA_STATUSCODE_GOOD) return (char*)UA_StatusCode_name( sc );

    return NULL;
}

static void freeRefArray(RefArray *a){
    for (int i = 0; i < a->used; i++){
        // We shuold use UA_ReferenceDescription_clear here,
        // but for some reason it crashes with:
        //   corrupted double-linked list
        // error.
        // Therefore we free the name, it seams enough, memory doesn't leak

        UA_LocalizedText_clear(&a->array[i].displayName);
    }
    free(a->array);
    a->array = NULL;
    a->size = 0;
    a->used = 0;
}