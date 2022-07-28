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

#ifndef eopcua_client_subscription__h
#define eopcua_client_browse_cache__h

#include <open62541/types.h>
#include <uthash.h>

typedef struct {
  char *path;
  int id;
  UT_hash_handle hh;
} node_index;

typedef struct {
  int id;
  UA_Variant *value;
  UA_StatusCode status;
  UA_DataType *type;
  UA_NodeId nodeId;
  UT_hash_handle hh;
} subscription_index; 

char *add_subscription(
    char *path, 
    int monId, 
    UA_NodeId *nodeId, 
    UA_Variant *nodeValue, 
    UA_StatusCode nodeStatus, 
    UA_DataType *nodeType
);

void monId2subscription(int monId, subscription_index **si);
char *path2subscription(char *path, subscription_index **si);

void purge_subscriptions(void);

#endif