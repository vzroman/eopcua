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

typedef struct {
  UA_ReferenceDescription *array;
  size_t step;
  size_t used;
  size_t size;
} RefArray;