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
#include <open62541/plugin/accesscontrol_default.h>
//----------------------------------------------------------
#include "utilities.h"
#include "opcua_server_config.h"

//-----------------------------------------------------------
// Encryption
//-----------------------------------------------------------
static void disableUnencrypted(UA_ServerConfig *config) {
    for(size_t i = 0; i < config->endpointsSize; i++) {
        UA_EndpointDescription *ep = &config->endpoints[i];
        if(ep->securityMode != UA_MESSAGESECURITYMODE_NONE)
            continue;

        UA_EndpointDescription_clear(ep);
        /* Move the last to this position */
        if(i + 1 < config->endpointsSize) {
            config->endpoints[i] = config->endpoints[config->endpointsSize-1];
            i--;
        }
        config->endpointsSize--;
    }
    /* Delete the entire array if the last Endpoint was removed */
    if(config->endpointsSize== 0) {
        UA_free(config->endpoints);
        config->endpoints = NULL;
    }
}

static char *configure_encryption(UA_ServerConfig *config, UA_Int16 port, cJSON* encryption){
    char *error = NULL;
    UA_StatusCode sc;

    UA_ByteString *ua_certificate = (UA_ByteString *)&UA_BYTESTRING_NULL;
    UA_ByteString *ua_privateKey = (UA_ByteString *)&UA_BYTESTRING_NULL;
    // Trust certificates in pem format
    size_t ua_trustListSize = 0;
    UA_ByteString *ua_trustList = NULL;
    // Ussuer certificates in pem format (not tested)
    size_t ua_issuerListSize = 0;
    UA_ByteString *ua_issuerList = NULL;
    // revocation list in pem format (not tested)
    UA_ByteString *ua_revocationList = NULL;
    size_t ua_revocationListSize = 0;

    // certificate
    cJSON *certificate = cJSON_GetObjectItemCaseSensitive(encryption, "certificate");
    if (!cJSON_IsString(certificate) || (certificate->valuestring == NULL)){
        error = "server certificate is not defined";
        // It is a secure connection, the key must be provided
        goto on_error;
    }
    ua_certificate = parse_base64( certificate->valuestring );
    if (ua_certificate == NULL){
        error = "unable to parse the certificate from base64";
        goto on_error;
    }

    // private key
    cJSON *privateKey = cJSON_GetObjectItemCaseSensitive(encryption, "private_key");
    if (!cJSON_IsString(privateKey) || (privateKey->valuestring == NULL)){
        error = "private key is not defined";
        goto on_error; 
    }
    ua_privateKey = parse_base64( privateKey->valuestring );
    if (ua_privateKey == NULL){
        error = "unable to parse the private key from base64";
        goto on_error;
    }

    cJSON *enableUnencrypted = cJSON_GetObjectItemCaseSensitive(encryption, "enable_unencrypted");

    cJSON *trustList = cJSON_GetObjectItemCaseSensitive(encryption, "trustList");
    if ( cJSON_IsArray(trustList) && cJSON_GetArraySize( trustList ) > 0 ) {
        error = base64_files(trustList, &ua_trustList);
        if (error) goto on_error;
        ua_trustListSize = cJSON_GetArraySize( trustList );
    }

    cJSON *issuerList = cJSON_GetObjectItemCaseSensitive(encryption, "issuerList");
    if ( cJSON_IsArray(issuerList) && cJSON_GetArraySize( issuerList ) > 0 ) {
        error = base64_files(issuerList, &ua_issuerList);
        if (error) goto on_error;
        ua_issuerListSize = cJSON_GetArraySize( issuerList );
    }

    cJSON *revocationList = cJSON_GetObjectItemCaseSensitive(encryption, "revocationList");
    if ( cJSON_IsArray(revocationList) && cJSON_GetArraySize( revocationList ) > 0 ) {
        error = base64_files(revocationList, &ua_revocationList);
        if (error) goto on_error;
        ua_revocationListSize = cJSON_GetArraySize( revocationList );
    }

    config->serverCertificate = *ua_certificate;
    sc = UA_ServerConfig_setDefaultWithSecurityPolicies(config, port,
                                                       ua_certificate, ua_privateKey,
                                                       ua_trustList, ua_trustListSize,
                                                       ua_issuerList, ua_issuerListSize,
                                                       ua_revocationList, ua_revocationListSize);

    if (sc != UA_STATUSCODE_GOOD){
        error = (char *)UA_StatusCode_name( sc );
        goto on_error;
    }

    if (cJSON_IsBool(enableUnencrypted) && (enableUnencrypted->valueint != 0)){
        LOGWARNING("unencrypted connections allowed");
    }else{
        LOGINFO("disable unencrypted connections");
        disableUnencrypted( config );
    }

    return error;

on_error:
    if (ua_certificate) UA_ByteString_delete(ua_certificate);
    if (ua_privateKey) UA_ByteString_delete(ua_privateKey);
    if (ua_trustList) UA_free(ua_trustList);
    if (ua_issuerList) UA_free(ua_issuerList);
    if (ua_revocationList) UA_free(ua_revocationList);

    return error;

}

//-----------------------------------------------------------
// Access
//-----------------------------------------------------------
static void disableAnonymous(UA_ServerConfig *config) {
    for(size_t i = 0; i < config->endpointsSize; i++) {
        UA_EndpointDescription *ep = &config->endpoints[i];

        for(size_t j = 0; j < ep->userIdentityTokensSize; j++) {
            UA_UserTokenPolicy *utp = &ep->userIdentityTokens[j];
            if(utp->tokenType != UA_USERTOKENTYPE_ANONYMOUS)
                continue;

            UA_UserTokenPolicy_clear(utp);
            /* Move the last to this position */
            if(j + 1 < ep->userIdentityTokensSize) {
                ep->userIdentityTokens[j] = ep->userIdentityTokens[ep->userIdentityTokensSize-1];
                j--;
            }
            ep->userIdentityTokensSize--;
        }

        /* Delete the entire array if the last UserTokenPolicy was removed */
        if(ep->userIdentityTokensSize == 0) {
            UA_free(ep->userIdentityTokens);
            ep->userIdentityTokens = NULL;
        }
    }
}

static char *configure_access(UA_ServerConfig *config, cJSON* access){
    char *error = NULL;
    UA_StatusCode sc;

    cJSON *users = cJSON_GetObjectItemCaseSensitive(access, "users");
    cJSON *enableAnonymous = cJSON_GetObjectItemCaseSensitive(access, "enable_anonymous");
    UA_UsernamePasswordLogin *ua_users = NULL;

    bool _enableAnonymous = cJSON_IsBool( enableAnonymous ) && (enableAnonymous->valueint != 0);

    if (!cJSON_IsArray(users) && !_enableAnonymous) {
        error = "users are not configured";
        goto on_error;
    }

    size_t count = cJSON_GetArraySize( users );
    if (count == 0 && !enableAnonymous){
        error = "no users are configured";
        goto on_error;
    }

    ua_users = (UA_UsernamePasswordLogin*)
        UA_malloc(count * sizeof(UA_UsernamePasswordLogin));
    
    if(!ua_users) {
        error = "out of memory";
        goto on_error;
    }
    for(size_t i = 0; i < count; i++) {
        cJSON *user = cJSON_GetArrayItem(users, i); 
        if (!cJSON_IsObject(user)){
            error = "invalid user format";
            goto on_error;
        }

        cJSON *login = cJSON_GetObjectItemCaseSensitive(user, "login");
        if (!cJSON_IsString(login) || (login->valuestring == NULL)){
            error = "user login is not defined";
            goto on_error; 
        }

        cJSON *password = cJSON_GetObjectItemCaseSensitive(user, "password");
        if (!cJSON_IsString(password) || (password->valuestring == NULL)){
            error = "user password is not defined";
            goto on_error; 
        }

        UA_String ua_login = UA_STRING_ALLOC(login->valuestring);
        UA_String ua_password = UA_STRING_ALLOC(password->valuestring);

        sc = UA_String_copy(&ua_login, &ua_users[i].username);
        if (sc != UA_STATUSCODE_GOOD){
            error = (char *)UA_StatusCode_name( sc );
            goto on_error;
        }

        sc = UA_String_copy(&ua_password, &ua_users[i].password);
        if (sc != UA_STATUSCODE_GOOD){
            error = (char *)UA_StatusCode_name( sc );
            goto on_error;
        }

    }

    // Update the config
    config->accessControl.clear(&config->accessControl);
    sc = UA_AccessControl_default(config, _enableAnonymous, NULL,
             &config->securityPolicies[config->securityPoliciesSize-1].policyUri, count, ua_users);
    
    if (sc != UA_STATUSCODE_GOOD){
        error = (char *)UA_StatusCode_name( sc );
        goto on_error;
    }

    if (_enableAnonymous){
        LOGWARNING("anonymous access allowed");
    }else{
        LOGINFO("disable anonymous access");
        disableAnonymous( config );
    }

    return error; 

on_error:
    if (ua_users) UA_free(ua_users);
    return error;
}

//-----------------------------------------------------------
// Description
//-----------------------------------------------------------
static char *configure_description(UA_ServerConfig *config, cJSON* description){
    char *error = NULL;

    cJSON *productName = cJSON_GetObjectItemCaseSensitive(description, "productName");
    if (cJSON_IsString(productName) && (productName->valuestring != NULL)){
        config->buildInfo.productName = UA_STRING_ALLOC(productName->valuestring);
        config->applicationDescription.applicationName = UA_LOCALIZEDTEXT_ALLOC("en-US",productName->valuestring);
    }

    cJSON *productUri = cJSON_GetObjectItemCaseSensitive(description, "productUri");
    if (cJSON_IsString(productUri) && (productUri->valuestring != NULL)){
        config->buildInfo.productUri = UA_STRING_ALLOC(productUri->valuestring);
        config->applicationDescription.productUri = UA_STRING_ALLOC(productUri->valuestring);
    }

    cJSON *manufacturerName = cJSON_GetObjectItemCaseSensitive(description, "manufacturerName");
    if (cJSON_IsString(manufacturerName) && (manufacturerName->valuestring != NULL)){
        config->buildInfo.manufacturerName = UA_STRING_ALLOC(manufacturerName->valuestring);
    }

    cJSON *softwareVersion = cJSON_GetObjectItemCaseSensitive(description, "softwareVersion");
    if (cJSON_IsString(softwareVersion) && (softwareVersion->valuestring != NULL)){
        config->buildInfo.softwareVersion = UA_STRING_ALLOC(softwareVersion->valuestring);
    }

    if (config->serverCertificate.length){
        char *appURI = parse_certificate_uri( &config->serverCertificate, &error );
        if (appURI != NULL){
            config->applicationDescription.applicationUri = UA_STRING_ALLOC( appURI );
            free( appURI );
        } 
    }else{
        // If no certificate provided take he application URI from the provided args
        cJSON *applicationUri = cJSON_GetObjectItemCaseSensitive(description, "applicationUri");
        if (cJSON_IsString(applicationUri) && (applicationUri->valuestring != NULL)){
            config->applicationDescription.applicationUri = UA_STRING_ALLOC(applicationUri->valuestring);
        }
    }

    return error;
}

//-----------------------------------------------------------
// Description
//-----------------------------------------------------------
static char *configure_limits(UA_ServerConfig *config, cJSON* limits){
    char *error = NULL;

    cJSON *maxSecureChannels = cJSON_GetObjectItemCaseSensitive(limits, "maxSecureChannels");
    if (cJSON_IsNumber(maxSecureChannels)){
        config->maxSecureChannels = (UA_UInt16) maxSecureChannels->valuedouble;
    }

    cJSON *maxSecurityTokenLifetime = cJSON_GetObjectItemCaseSensitive(limits, "maxSecurityTokenLifetime");
    if (cJSON_IsNumber(maxSecurityTokenLifetime)){
        config->maxSecurityTokenLifetime = (UA_UInt32) maxSecurityTokenLifetime->valuedouble;
    }

    cJSON *maxSessions = cJSON_GetObjectItemCaseSensitive(limits, "maxSessions");
    if (cJSON_IsNumber(maxSessions)){
        config->maxSessions = (UA_UInt16) maxSessions->valuedouble;
    }

    cJSON *maxSessionTimeout = cJSON_GetObjectItemCaseSensitive(limits, "maxSessionTimeout");
    if (cJSON_IsNumber(maxSessionTimeout)){
        config->maxSessionTimeout = (UA_Double) maxSessionTimeout->valuedouble;
    }

    cJSON *maxNodesPerRead = cJSON_GetObjectItemCaseSensitive(limits, "maxNodesPerRead");
    if (cJSON_IsNumber(maxNodesPerRead)){
        config->maxNodesPerRead = (UA_UInt32) maxNodesPerRead->valuedouble;
    }

    cJSON *maxNodesPerWrite = cJSON_GetObjectItemCaseSensitive(limits, "maxNodesPerWrite");
    if (cJSON_IsNumber(maxNodesPerWrite)){
        config->maxNodesPerWrite = (UA_UInt32) maxNodesPerWrite->valuedouble;
    }

    return error;
}

// The entry point
char *configure(UA_ServerConfig *config, cJSON* args){
    char *error = NULL;
    UA_StatusCode sc;

    UA_Int16 ua_port = 4840;

    //----------options-----------------
    cJSON *port = cJSON_GetObjectItemCaseSensitive(args, "port");
    if ( cJSON_IsNumber(port) ){
        ua_port = (UA_Int16) port->valuedouble;
    }

    cJSON *encryption = cJSON_GetObjectItemCaseSensitive(args, "encryption");
    if (cJSON_IsObject( encryption )){
        // Encrypted
        error = configure_encryption(config, ua_port, encryption);
        if (error) goto on_error;
    }else{
        // No encryption
        sc = UA_ServerConfig_setMinimal(config, ua_port, NULL);
        if (sc != UA_STATUSCODE_GOOD){
            error = (char *)UA_StatusCode_name( sc );
            goto on_error;
        }
    }

    cJSON *access = cJSON_GetObjectItemCaseSensitive(args, "access");
    if ( cJSON_IsObject(access) ) {
        error = configure_access(config, access);
        if (error) goto on_error;
    }

    // Define custom host name
    cJSON *host = cJSON_GetObjectItemCaseSensitive(args, "host");
    if (cJSON_IsString(host) && (host->valuestring != NULL)){
        config->customHostname = UA_STRING_ALLOC( host->valuestring );
    }

    cJSON *description = cJSON_GetObjectItemCaseSensitive(args, "description");
    if (cJSON_IsObject(description)){
        error = configure_description(config, description);
        if (error) goto on_error;
    }

    cJSON *limits = cJSON_GetObjectItemCaseSensitive(args, "limits");
    if (cJSON_IsObject(limits)){
        error = configure_limits(config, limits);
        if (error) goto on_error;
    }


    return error;

on_error:
    UA_ServerConfig_clean(config);
    return error;
}

