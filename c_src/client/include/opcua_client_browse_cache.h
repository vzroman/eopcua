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

#ifndef eopcua_client_browse_cache__h
#define eopcua_client_browse_cache__h

#include <open62541/types.h>

char *add_cache(char *path, UA_NodeId *nodeId);
UA_NodeId *lookup_path2nodeId_cache(char *path);
char *lookup_nodeId2path_cache(UA_NodeId *nodeId);
char **get_all_cache_items(void);
void purge_cache(void);

#endif