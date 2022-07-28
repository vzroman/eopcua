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

#ifndef eopcua_server_loop__h
#define eopcua_server_loop__h

#include <open62541/types.h>
#include <eport_c.h>

char* start(cJSON *args);
void stop(void);
bool is_started(void);

char *add_variable(UA_NodeId folder, char *name, UA_NodeId *outNodeId);
char *add_folder(UA_NodeId folder, char *name, UA_NodeId *outNodeId);

char *write_value(UA_NodeId *nodeId, char *type, cJSON *value);
char *read_value(UA_NodeId *nodeId, cJSON **value);

#endif