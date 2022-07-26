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
#include <open62541/types_generated_handling.h>
#include "opcua_client_subscription.h"

// DEBUG:
#include <eport_c_log.h>
//-----------------------------------------------------
//  Subscription indexes
//-----------------------------------------------------
node_index *__node_index = NULL;
subscription_index *__subscription_index = NULL;

char *add_subscription(
    char *path, 
    int monId, 
    UA_NodeId *nodeId, 
    UA_Variant *nodeValue, 
    UA_StatusCode nodeStatus, 
    UA_DataType *nodeType
){
    char *error = NULL;

    // Add the binding to the collection
    node_index *ni = NULL;
    subscription_index *si = NULL;
    ni = (node_index *)malloc(sizeof(node_index));
    if (ni == NULL){
        error = "unable to allocate the memory for a new binding index";
        goto on_error;
    }
    ni->path = strdup( path );
    ni->id = monId;

    si = (subscription_index *)malloc(sizeof(subscription_index));
    if (si == NULL){
        error = "unable to allocate the memory for new binding";
        goto on_error;
    }
    UA_StatusCode sc = UA_NodeId_copy(nodeId, &si->nodeId);
    if (sc != UA_STATUSCODE_GOOD){
        error = (char*)UA_StatusCode_name( sc );
        goto on_error;
    }
    
    si->id = monId;
    si->value = nodeValue;
    si->status = nodeStatus;
    si->type = nodeType;

    HASH_ADD_STR(__node_index, path, ni);
    LOGINFO("DEBUG: add monId %d", si->id);
    HASH_ADD_INT(__subscription_index, id, si);

    return NULL;
on_error:
  if(ni) free(ni);
  if(si) free(si);
  return error;
}

void monId2subscription(int monId, subscription_index **si){
    HASH_FIND_INT(__subscription_index, &monId, *si);
}

char *path2subscription(char *path, subscription_index **si){
    
    node_index *ni = NULL;
    HASH_FIND_STR(__node_index, path, ni);
    if (!ni) goto on_error;
    
    monId2subscription(ni->id, si);
    if (!*si) goto on_error;

    return NULL;
on_error:
    return "invalid subscription";
}

void purge_subscriptions(){
    node_index *b;
    for (b = __node_index; b != NULL; b = b->hh.next) {
        HASH_DEL(__node_index, b);
        free( b->path );
        free( b );
    }
    __node_index = NULL;

    subscription_index *s;
    for (s = __subscription_index; s != NULL; s = s->hh.next) {
        HASH_DEL(__subscription_index, s); 
        UA_Variant_delete(s->value);
        UA_NodeId_delete( &s->nodeId );
        free( s );
    }
    __subscription_index = NULL;
}


