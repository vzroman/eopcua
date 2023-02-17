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

#ifndef eopcua_client_loop__h
#define eopcua_client_loop__h

#include <eport_c.h>

char *start(char *url, char *certificate, char *privateKey, char *login, char *pass, int cycle, size_t maxNodesPerBrowse);
void stop(void);
bool is_started(void);

char* browse_servers(char *host, int port, char ***urls);

char *read_values(size_t size, UA_NodeId **nodeId, UA_DataValue** values);
char *write_values(size_t size, UA_NodeId **nodeId, UA_Variant **values, char ***results);


#endif