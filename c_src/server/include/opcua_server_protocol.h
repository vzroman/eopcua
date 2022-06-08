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
#include <cjson/cJSON.h>

typedef unsigned long TID;

// Command types
typedef enum OPCUA_SERVER_CMD_T {
    OPCUA_SERVER_START
} OPCUA_SERVER_CMD;

// Request
typedef struct opcua_server_request{
    OPCUA_SERVER_CMD cmd;
    TID tid;
    cJSON *body;
} OPCUA_SERVER_REQUEST;

// Parse a request
int parse_request( const char *message, OPCUA_SERVER_REQUEST* request );
void purge_request( OPCUA_SERVER_REQUEST *request );

// Build response
char* create_response( OPCUA_SERVER_REQUEST *request, cJSON *response );