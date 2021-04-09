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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <open62541/client_config_default.h>
#include <open62541/client_highlevel.h>
#include <open62541/client_subscriptions.h>
#include <open62541/plugin/log_stdout.h>
#include "opcua_client.h"
#include "opcua_client_protocol.h"
 
cJSON* on_error(char* text);
cJSON* on_ok(cJSON* response);
cJSON* opcua_client_connect(cJSON* request);
cJSON* opcua_client_read(cJSON* request);

int path2nodeId( cJSON *path, UA_NodeId *node );
cJSON* browse_folder( UA_NodeId folder );

// Global variables
UA_Client *opcua_client;

char* on_request( char *requestString ){
    cJSON *response;
    char *responseString;

    // Parse the request
    fprintf(stdout,"DEBUG: parsing the request\r\n");
    OPCUA_CLIENT_REQUEST *request = parse_request( requestString );

    // Handle the request
    fprintf(stdout,"DEBUG: handle the request\r\n");
    if (request == NULL){
        response = on_error("invalid request");
    } else if( request->cmd == OPCUA_CLIENT_CONNECT ){
        response = opcua_client_connect( request->body );
    } else if (request->cmd == OPCUA_CLIENT_READ ){
        response = opcua_client_read( request->body );
    } else{
        response = on_error("unsupported command type");
    }

    // Reply
    responseString = create_response( request, response );
    fprintf(stdout,"DEBUG: response %s\r\n",responseString);
    purge_request( request );

    return responseString;
}

cJSON* on_error(char* text){
    cJSON *response = cJSON_CreateObject();
    if (cJSON_AddStringToObject(response, "type", "error") == NULL) {
        goto error;
    }
    if (cJSON_AddStringToObject(response, "text", text) == NULL) {
        goto error;
    }
    return response;

error:
    cJSON_Delete( response );
    return NULL;
}

cJSON* on_ok(cJSON *result){
    cJSON *response = cJSON_CreateObject();
    if ( cJSON_AddStringToObject(response, "type", "ok") == NULL) {
        goto error;
    }
    if ( !cJSON_AddItemToObject(response, "result", result) ) {
        goto error;
    }
    return response;

error:
    cJSON_Delete( response );
    return NULL;
}

cJSON* opcua_client_connect(cJSON* request){
    char *errorString = NULL;
    UA_StatusCode retval;

    // Connection params
    cJSON *url = cJSON_GetObjectItemCaseSensitive(request, "url");
    cJSON *login = cJSON_GetObjectItemCaseSensitive(request, "login");
    cJSON *password = cJSON_GetObjectItemCaseSensitive(request, "password");
    if (!cJSON_IsString(login) || (login->valuestring == NULL)){
        login = NULL;
        password = NULL; 
    }

    if (login != NULL && password != NULL){
        // Authorized access
        fprintf(stdout,"connecting to %s, user %s\r\n", url->valuestring,login->valuestring);
        retval = UA_Client_connectUsername(opcua_client, url->valuestring, login->valuestring, password->valuestring);
    }else{
        fprintf(stdout,"connecting to %s\r\n", url->valuestring);
        retval = UA_Client_connect(opcua_client, url->valuestring);
    }

    if(retval != UA_STATUSCODE_GOOD) {
        errorString = "connection error";
        goto error;
    }

    cJSON *response = cJSON_CreateString("ok");
    if (response == NULL){
        goto error;
    }

    return on_ok( response );

error:
    cJSON_Delete( response );
    if (errorString == NULL){
        errorString = "programming error in opcua_client_connect";
    }
    return on_error( errorString );
}

cJSON* opcua_client_read(cJSON* request){
    char *errorString = NULL;
    fprintf(stdout,"DEBUG: reading\r\n");

    UA_NodeId nodeId = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
    int ret = path2nodeId( request, &nodeId );

    if (ret != 0){
        errorString = "invalid node path";
        goto error;
    }

    //retval = UA_Client_readValueAttribute(client, UA_NODEID_STRING(1, "the.answer"), val);

    cJSON *response = cJSON_CreateString("TODO: read");
    if (response == NULL){
        goto error;
    }
    return response;

error:
    cJSON_Delete( response );
    if (errorString == NULL){
        errorString = "programming error in opcua_client_read";
    }
    return on_error( errorString );
}

int path2nodeId( cJSON *path, UA_NodeId *node ){
    cJSON *level = NULL;
    cJSON *content = NULL;
    cJSON *next = NULL;

    // Start from the Objects folder
    cJSON_ArrayForEach(level, path) {
        content = browse_folder( *node );
        if (content == NULL){
            fprintf(stdout,"ERROR: path2nodeId unable to browse folder\r\n");
            goto error;
        }

        // Lookup node by name
        next = cJSON_GetObjectItemCaseSensitive(content, level->valuestring );
        if (!cJSON_IsString(next) || (next->valuestring == NULL)){
            fprintf(stdout,"ERROR: path2nodeId invalid level %s\r\n",level->valuestring);
            goto error;
        }

        if (UA_NodeId_parse(node, UA_STRING((char*)(uintptr_t)next->valuestring)) != UA_STATUSCODE_GOOD){
            fprintf(stdout,"ERROR: path2nodeId unable to parse nodeId %s\r\n",next->valuestring);
            goto error;
        }
        cJSON_Delete( content );
    }

    return 0;

error:
    cJSON_Delete( content );
    return -1;
}

cJSON* browse_folder( UA_NodeId folder ){
    cJSON *result = NULL;
    UA_String nodeId;

    // Build the request
    UA_BrowseRequest request;
    UA_BrowseRequest_init(&request);
    request.requestedMaxReferencesPerNode = 0;
    request.nodesToBrowse = UA_BrowseDescription_new();
    request.nodesToBrowseSize = 1;
    request.nodesToBrowse[0].nodeId = folder; 
    request.nodesToBrowse[0].resultMask = UA_BROWSERESULTMASK_ALL;

    // Execute the request
    UA_BrowseResponse response = UA_Client_Service_browse(opcua_client, request);
    if (response.responseHeader.serviceResult != UA_STATUSCODE_GOOD){
        fprintf(stdout,"ERROR: UA_Client_Service_browse error\r\n");
        goto error;
    }

    // Build the result
    result = cJSON_CreateObject();
    for(size_t i = 0; i < response.resultsSize; ++i) {
        for(size_t j = 0; j < response.results[i].referencesSize; ++j) {

            UA_ReferenceDescription *ref = &(response.results[i].references[j]);

            // Convert the nodeId to string
            if (UA_NodeId_print(&ref->nodeId.nodeId, &nodeId) != UA_STATUSCODE_GOOD){
                fprintf(stdout,"ERROR: unable to serialize nodeId in browse_folder\r\n");
                goto error; 
            }
            
            if (cJSON_AddStringToObject(result, (char *)ref->displayName.text.data, (char *)nodeId.data) == NULL) {
                fprintf(stdout,"ERROR: unable to add a node the result in browse_folder\r\n");
                UA_String_clear(&nodeId);
                goto error; 
            }
            UA_String_clear(&nodeId);
        }
    }
    UA_BrowseRequest_clear(&request);
    UA_BrowseResponse_clear(&response);

    return result;

error:
    cJSON_Delete( result );
    UA_BrowseRequest_clear(&request);
    UA_BrowseResponse_clear(&response);
    return NULL;
}

// #ifdef UA_ENABLE_SUBSCRIPTIONS
// static void
// handler_TheAnswerChanged(UA_Client *client, UA_UInt32 subId, void *subContext,
//                          UA_UInt32 monId, void *monContext, UA_DataValue *value) {
//     printf("The Answer has changed!\n");
// }
// #endif
 
// static UA_StatusCode
// nodeIter(UA_NodeId childId, UA_Boolean isInverse, UA_NodeId referenceTypeId, void *handle) {
//     if(isInverse)
//         return UA_STATUSCODE_GOOD;
//     UA_NodeId *parent = (UA_NodeId *)handle;
//     printf("%d, %d --- %d ---> NodeId %d, %d\n",
//            parent->namespaceIndex, parent->identifier.numeric,
//            referenceTypeId.identifier.numeric, childId.namespaceIndex,
//            childId.identifier.numeric);
//     return UA_STATUSCODE_GOOD;
// }

int main(int argc, char *argv[]) {
    // Create an object
    opcua_client = UA_Client_new();
    if (opcua_client == NULL){
        fprintf(stdout,"unable to initialize the connection object\r\n");
        exit(EXIT_FAILURE);
    }
    UA_ClientConfig_setDefault(UA_Client_getConfig(opcua_client));

    printf("enter eport_loop\r\n");
    eport_loop( &on_request );
//     UA_Client *client = UA_Client_new();
//     UA_ClientConfig_setDefault(UA_Client_getConfig(client));
 
//     /* Listing endpoints */
//     UA_EndpointDescription* endpointArray = NULL;
//     size_t endpointArraySize = 0;
//     UA_StatusCode retval = UA_Client_getEndpoints(client, "opc.tcp://localhost:4840",
//                                                   &endpointArraySize, &endpointArray);
//     if(retval != UA_STATUSCODE_GOOD) {
//         UA_Array_delete(endpointArray, endpointArraySize, &UA_TYPES[UA_TYPES_ENDPOINTDESCRIPTION]);
//         UA_Client_delete(client);
//         return EXIT_FAILURE;
//     }
//     printf("%i endpoints found\n", (int)endpointArraySize);
//     for(size_t i=0;i<endpointArraySize;i++) {
//         printf("URL of endpoint %i is %.*s\n", (int)i,
//                (int)endpointArray[i].endpointUrl.length,
//                endpointArray[i].endpointUrl.data);
//     }
//     UA_Array_delete(endpointArray,endpointArraySize, &UA_TYPES[UA_TYPES_ENDPOINTDESCRIPTION]);
 
//     /* Connect to a server */
//     // This one is for authorized connections
//     //retval = UA_Client_connectUsername(client, "opc.tcp://localhost:4840", "user1", "password");

//     // This one is for anonimous connections
//     retval = UA_Client_connect(client, "opc.tcp://localhost:4840/OPCUA/SimulationServer");

//     if(retval != UA_STATUSCODE_GOOD) {
//         UA_Client_delete(client);
//         return EXIT_FAILURE;
//     }
 
//     /* Browse some objects */
//     printf("Browsing nodes in objects folder:\n");
//     UA_BrowseRequest bReq;
//     UA_BrowseRequest_init(&bReq);
//     bReq.requestedMaxReferencesPerNode = 0;
//     bReq.nodesToBrowse = UA_BrowseDescription_new();
//     bReq.nodesToBrowseSize = 1;
//     bReq.nodesToBrowse[0].nodeId = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER); /* browse objects folder */
//     bReq.nodesToBrowse[0].resultMask = UA_BROWSERESULTMASK_ALL; /* return everything */
//     UA_BrowseResponse bResp = UA_Client_Service_browse(client, bReq);
//     printf("%-9s %-16s %-16s %-16s\n", "NAMESPACE", "NODEID", "BROWSE NAME", "DISPLAY NAME");
//     for(size_t i = 0; i < bResp.resultsSize; ++i) {
//         for(size_t j = 0; j < bResp.results[i].referencesSize; ++j) {
//             UA_ReferenceDescription *ref = &(bResp.results[i].references[j]);
//             if(ref->nodeId.nodeId.identifierType == UA_NODEIDTYPE_NUMERIC) {
//                 printf("%-9d %-16d %-16.*s %-16.*s\n", ref->nodeId.nodeId.namespaceIndex,
//                        ref->nodeId.nodeId.identifier.numeric, (int)ref->browseName.name.length,
//                        ref->browseName.name.data, (int)ref->displayName.text.length,
//                        ref->displayName.text.data);
//             } else if(ref->nodeId.nodeId.identifierType == UA_NODEIDTYPE_STRING) {
//                 printf("%-9d %-16.*s %-16.*s %-16.*s\n", ref->nodeId.nodeId.namespaceIndex,
//                        (int)ref->nodeId.nodeId.identifier.string.length,
//                        ref->nodeId.nodeId.identifier.string.data,
//                        (int)ref->browseName.name.length, ref->browseName.name.data,
//                        (int)ref->displayName.text.length, ref->displayName.text.data);
//             }
//             /* TODO: distinguish further types */
//         }
//     }
//     UA_BrowseRequest_clear(&bReq);
//     UA_BrowseResponse_clear(&bResp);
 
//     /* Same thing, this time using the node iterator... */
//     UA_NodeId *parent = UA_NodeId_new();
//     *parent = UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER);
//     UA_Client_forEachChildNodeCall(client, UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER),
//                                    nodeIter, (void *) parent);
//     UA_NodeId_delete(parent);
 
// #ifdef UA_ENABLE_SUBSCRIPTIONS
//     /* Create a subscription */
//     UA_CreateSubscriptionRequest request = UA_CreateSubscriptionRequest_default();
//     UA_CreateSubscriptionResponse response = UA_Client_Subscriptions_create(client, request,
//                                                                             NULL, NULL, NULL);
 
//     UA_UInt32 subId = response.subscriptionId;
//     if(response.responseHeader.serviceResult == UA_STATUSCODE_GOOD)
//         printf("Create subscription succeeded, id %u\n", subId);
 
//     UA_MonitoredItemCreateRequest monRequest =
//         UA_MonitoredItemCreateRequest_default(UA_NODEID_STRING(1, "the.answer"));
 
//     UA_MonitoredItemCreateResult monResponse =
//     UA_Client_MonitoredItems_createDataChange(client, response.subscriptionId,
//                                               UA_TIMESTAMPSTORETURN_BOTH,
//                                               monRequest, NULL, handler_TheAnswerChanged, NULL);
//     if(monResponse.statusCode == UA_STATUSCODE_GOOD)
//         printf("Monitoring 'the.answer', id %u\n", monResponse.monitoredItemId);
 
 
//     /* The first publish request should return the initial value of the variable */
//     UA_Client_run_iterate(client, 1000);
// #endif
 
//     /* Read attribute */
//     UA_Int32 value = 0;
//     printf("\nReading the value of node (1, \"the.answer\"):\n");
//     UA_Variant *val = UA_Variant_new();
//     retval = UA_Client_readValueAttribute(client, UA_NODEID_STRING(1, "the.answer"), val);
//     if(retval == UA_STATUSCODE_GOOD && UA_Variant_isScalar(val) &&
//        val->type == &UA_TYPES[UA_TYPES_INT32]) {
//             value = *(UA_Int32*)val->data;
//             printf("the value is: %i\n", value);
//     }
//     UA_Variant_delete(val);
 
//     /* Write node attribute */
//     value++;
//     printf("\nWriting a value of node (1, \"the.answer\"):\n");
//     UA_WriteRequest wReq;
//     UA_WriteRequest_init(&wReq);
//     wReq.nodesToWrite = UA_WriteValue_new();
//     wReq.nodesToWriteSize = 1;
//     wReq.nodesToWrite[0].nodeId = UA_NODEID_STRING_ALLOC(1, "the.answer");
//     wReq.nodesToWrite[0].attributeId = UA_ATTRIBUTEID_VALUE;
//     wReq.nodesToWrite[0].value.hasValue = true;
//     wReq.nodesToWrite[0].value.value.type = &UA_TYPES[UA_TYPES_INT32];
//     wReq.nodesToWrite[0].value.value.storageType = UA_VARIANT_DATA_NODELETE; /* do not free the integer on deletion */
//     wReq.nodesToWrite[0].value.value.data = &value;
//     UA_WriteResponse wResp = UA_Client_Service_write(client, wReq);
//     if(wResp.responseHeader.serviceResult == UA_STATUSCODE_GOOD)
//             printf("the new value is: %i\n", value);
//     UA_WriteRequest_clear(&wReq);
//     UA_WriteResponse_clear(&wResp);
 
//     /* Write node attribute (using the highlevel API) */
//     value++;
//     UA_Variant *myVariant = UA_Variant_new();
//     UA_Variant_setScalarCopy(myVariant, &value, &UA_TYPES[UA_TYPES_INT32]);
//     UA_Client_writeValueAttribute(client, UA_NODEID_STRING(1, "the.answer"), myVariant);
//     UA_Variant_delete(myVariant);
 
// #ifdef UA_ENABLE_SUBSCRIPTIONS
//     /* Take another look at the.answer */
//     UA_Client_run_iterate(client, 100);
//     /* Delete the subscription */
//     if(UA_Client_Subscriptions_deleteSingle(client, subId) == UA_STATUSCODE_GOOD)
//         printf("Subscription removed\n");
// #endif
 
// #ifdef UA_ENABLE_METHODCALLS
//     /* Call a remote method */
//     UA_Variant input;
//     UA_String argString = UA_STRING("Hello Server");
//     UA_Variant_init(&input);
//     UA_Variant_setScalarCopy(&input, &argString, &UA_TYPES[UA_TYPES_STRING]);
//     size_t outputSize;
//     UA_Variant *output;
//     retval = UA_Client_call(client, UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER),
//                             UA_NODEID_NUMERIC(1, 62541), 1, &input, &outputSize, &output);
//     if(retval == UA_STATUSCODE_GOOD) {
//         printf("Method call was successful, and %lu returned values available.\n",
//                (unsigned long)outputSize);
//         UA_Array_delete(output, outputSize, &UA_TYPES[UA_TYPES_VARIANT]);
//     } else {
//         printf("Method call was unsuccessful, and %x returned values available.\n", retval);
//     }
//     UA_Variant_clear(&input);
// #endif
 
// #ifdef UA_ENABLE_NODEMANAGEMENT
//     /* Add new nodes*/
//     /* New ReferenceType */
//     UA_NodeId ref_id;
//     UA_ReferenceTypeAttributes ref_attr = UA_ReferenceTypeAttributes_default;
//     ref_attr.displayName = UA_LOCALIZEDTEXT("en-US", "NewReference");
//     ref_attr.description = UA_LOCALIZEDTEXT("en-US", "References something that might or might not exist");
//     ref_attr.inverseName = UA_LOCALIZEDTEXT("en-US", "IsNewlyReferencedBy");
//     retval = UA_Client_addReferenceTypeNode(client,
//                                             UA_NODEID_NUMERIC(1, 12133),
//                                             UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES),
//                                             UA_NODEID_NUMERIC(0, UA_NS0ID_HASSUBTYPE),
//                                             UA_QUALIFIEDNAME(1, "NewReference"),
//                                             ref_attr, &ref_id);
//     if(retval == UA_STATUSCODE_GOOD )
//         printf("Created 'NewReference' with numeric NodeID %u\n", ref_id.identifier.numeric);
 
//     /* New ObjectType */
//     UA_NodeId objt_id;
//     UA_ObjectTypeAttributes objt_attr = UA_ObjectTypeAttributes_default;
//     objt_attr.displayName = UA_LOCALIZEDTEXT("en-US", "TheNewObjectType");
//     objt_attr.description = UA_LOCALIZEDTEXT("en-US", "Put innovative description here");
//     retval = UA_Client_addObjectTypeNode(client,
//                                          UA_NODEID_NUMERIC(1, 12134),
//                                          UA_NODEID_NUMERIC(0, UA_NS0ID_BASEOBJECTTYPE),
//                                          UA_NODEID_NUMERIC(0, UA_NS0ID_HASSUBTYPE),
//                                          UA_QUALIFIEDNAME(1, "NewObjectType"),
//                                          objt_attr, &objt_id);
//     if(retval == UA_STATUSCODE_GOOD)
//         printf("Created 'NewObjectType' with numeric NodeID %u\n", objt_id.identifier.numeric);
 
//     /* New Object */
//     UA_NodeId obj_id;
//     UA_ObjectAttributes obj_attr = UA_ObjectAttributes_default;
//     obj_attr.displayName = UA_LOCALIZEDTEXT("en-US", "TheNewGreatNode");
//     obj_attr.description = UA_LOCALIZEDTEXT("de-DE", "Hier koennte Ihre Webung stehen!");
//     retval = UA_Client_addObjectNode(client,
//                                      UA_NODEID_NUMERIC(1, 0),
//                                      UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER),
//                                      UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES),
//                                      UA_QUALIFIEDNAME(1, "TheGreatNode"),
//                                      UA_NODEID_NUMERIC(1, 12134),
//                                      obj_attr, &obj_id);
//     if(retval == UA_STATUSCODE_GOOD )
//         printf("Created 'NewObject' with numeric NodeID %u\n", obj_id.identifier.numeric);
 
//     /* New Integer Variable */
//     UA_NodeId var_id;
//     UA_VariableAttributes var_attr = UA_VariableAttributes_default;
//     var_attr.displayName = UA_LOCALIZEDTEXT("en-US", "TheNewVariableNode");
//     var_attr.description =
//         UA_LOCALIZEDTEXT("en-US", "This integer is just amazing - it has digits and everything.");
//     UA_Int32 int_value = 1234;
//     /* This does not copy the value */
//     UA_Variant_setScalar(&var_attr.value, &int_value, &UA_TYPES[UA_TYPES_INT32]);
//     var_attr.dataType = UA_TYPES[UA_TYPES_INT32].typeId;
//     retval = UA_Client_addVariableNode(client,
//                                        UA_NODEID_NUMERIC(1, 0), // Assign new/random NodeID
//                                        UA_NODEID_NUMERIC(0, UA_NS0ID_OBJECTSFOLDER),
//                                        UA_NODEID_NUMERIC(0, UA_NS0ID_ORGANIZES),
//                                        UA_QUALIFIEDNAME(0, "VariableNode"),
//                                        UA_NODEID_NULL, // no variable type
//                                        var_attr, &var_id);
//     if(retval == UA_STATUSCODE_GOOD )
//         printf("Created 'NewVariable' with numeric NodeID %u\n", var_id.identifier.numeric);
// #endif
 
//     UA_Client_disconnect(client);
//     UA_Client_delete(client);
    return EXIT_SUCCESS;
}