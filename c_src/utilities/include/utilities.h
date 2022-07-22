/*----------------------------------------------------------------
* Copyright (c) 2022 Faceplate
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
#include <open62541/types.h>
#include <open62541/types_generated.h>
#include <open62541/types_generated_handling.h>
#include <open62541/util.h>
//----------------------------------------
#include <cjson/cJSON.h>

#ifndef eopcua_utilities__h
#define eopcua_utilities__h

char** str_split(char* a_str, const char a_delim);
void str_split_destroy(char** tokens);
char* str_replace(const char* source, const char* search, const char* replace);

UA_ByteString* parse_base64(char* base64string);
char *base64_files(cJSON *files, UA_ByteString **result);
UA_ByteString loadFile(const char* path);
char* parse_certificate_uri(const UA_ByteString *certificate, char **error);

cJSON* ua2json( const UA_DataType *type, void *value );
UA_Variant *json2ua(const UA_DataType *type, cJSON *value);
const UA_DataType *type2ua(const char *type );

#endif
