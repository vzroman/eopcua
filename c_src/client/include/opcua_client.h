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