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

// The header of the request that defines the total lengsth of the message
#define HEADER_LENGTH 4

// Input file descriptor for receiving requests from erlang
#define IN_DESC 3

// Output file descriptor for sending response to erlang
#define OUT_DESC 4

// Debug info
//#define EPORT_DEBUG 1

#ifdef EPORT_DEBUG

#define LOGDEBUG(...) do{ fprintf(stdout,__VA_ARGS__); } while(0)

#else

#define LOGDEBUG(...) do{  } while(0)

#endif

#define LOGERROR(...) do{ fprintf(stdout,__VA_ARGS__); } while(0)

typedef unsigned char byte;

// Callback type 
typedef char* (*eport_request_handler) (char *);

// The loop definition
void eport_loop( eport_request_handler callback );
