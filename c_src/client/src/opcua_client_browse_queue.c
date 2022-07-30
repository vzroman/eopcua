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

#include "opcua_client_browse_queue.h"
//-----------------------------------------------------
//  Cache
//-----------------------------------------------------
typedef struct {
  char *path;
  UT_hash_handle hh;
} browse_queue_entry;


browse_queue_entry *__browse_queue = NULL;

char *add_browse_queue(char *path){

    browse_queue_entry *entry = NULL;
    HASH_FIND_STR(__browse_queue, path, entry);
    if (entry) return NULL;

    entry = (browse_queue_entry *)malloc( sizeof(browse_queue_entry));
    if (!entry) return "out of memory";

    entry->path = strdup( path );

    HASH_ADD_STR(__browse_queue, path, entry);

    return NULL;
}

char **get_browse_queue(size_t *size){

    *size = HASH_CNT(hh, __browse_queue);
    char **entries = (char **)malloc( sizeof(char *) * (*size) );

    if(!entries) return NULL;

    browse_queue_entry *entry; size_t i = 0;
    for (entry= __browse_queue; entry != NULL; entry = entry->hh.next) {
        entries[i++] = entry->path;
    }

    return entries;
}

void purge_browse_queue(){

    browse_queue_entry *entry;
    for (entry= __browse_queue; entry != NULL; entry = entry->hh.next) {
        free( entry->path );
        HASH_DEL(__browse_queue, entry);
        free( entry );
    }

    __browse_queue = NULL;
}



