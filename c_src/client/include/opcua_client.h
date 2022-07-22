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
} opcua_client_cache;

opcua_client_cache *__browse_cache;

static char *add_cache(char *path, UA_NodeId *nodeId){
    char *error = NULL;
    opcua_client_cache *c = (opcua_client_cache *)malloc( sizeof(opcua_client_cache) );
    if (!c){
        error = "unable to allocate the memory for a cache entry";
        goto on_error;
    }
    c->path = strdup(path);
    UA_StatusCode sc = UA_NodeId_copy(nodeId, &c->nodeId);
    if (sc != UA_STATUSCODE_GOOD){
        error = (char*)UA_StatusCode_name( sc );
        goto on_error;
    }

    HASH_ADD_STR(__browse_cache, path, c);

    return NULL;

on_error:
  if (c) free(c);
  return error;
}

static UA_NodeId *lookup_cache(char *path){
    opcua_client_cache *c = NULL;
    HASH_FIND_STR(__browse_cache, path, c);
    if (c == NULL){
        return NULL;
    }else{
       return &c->nodeId;
    }
}

static void purge_cache(){
    opcua_client_cache *c;
    for (c= __browse_cache; c != NULL; c = c->hh.next) {
        HASH_DEL(__browse_cache, c);
        free( c->path );
        free( c );
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