
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

#include <stdlib.h>
#include <assert.h>
#include <string.h>
//----------------------------------------
#include <openssl/x509v3.h>
#include <openssl/bn.h>
#include <openssl/asn1.h>
#include <openssl/x509.h>
#include <openssl/x509_vfy.h>
#include <openssl/pem.h>
#include <openssl/bio.h>
//----------------------------------------
#include "utilities.h"

bool uaIsNumber( const UA_DataType *type );

char** str_split(char* a_str, const char a_delim){
    char** result    = 0;
    size_t count     = 0;
    char* tmp        = a_str;
    char* last_comma = 0;
    char delim[2];
    delim[0] = a_delim;
    delim[1] = 0;

    /* Count how many elements will be extracted. */
    while (*tmp)
    {
        if (a_delim == *tmp)
        {
            count++;
            last_comma = tmp;
        }
        tmp++;
    }

    /* Add space for trailing token. */
    count += last_comma < (a_str + strlen(a_str) - 1);

    /* Add space for terminating null string so caller
       knows where the list of returned strings ends. */
    count++;

    result = malloc(sizeof(char*) * count);

    if (result)
    {
        size_t idx  = 0;
        char* token = strtok(a_str, delim);

        while (token)
        {
            assert(idx < count);
            *(result + idx++) = strdup(token);
            token = strtok(0, delim);
        }
        assert(idx == count - 1);
        *(result + idx) = 0;
    }

    return result;
}

void str_split_destroy(char** tokens){
    if (tokens){
        for (int i = 0; *(tokens + i); i++){
            free(*(tokens + i));
        }
        free(tokens);
    }
}

UA_ByteString* parse_base64(char* base64string){
    UA_ByteString *result = UA_ByteString_new();

    UA_String b64 = UA_STRING_ALLOC( base64string );

    if ( UA_ByteString_fromBase64( result, &b64 ) != UA_STATUSCODE_GOOD){
        goto error;
    };
    UA_String_clear( &b64 );

	return result;

error:
    UA_ByteString_clear( result );
    UA_String_clear( &b64 );
    return NULL;
}

char* parse_certificate_uri(UA_ByteString *certificate, char **error){
    X509 *cert = NULL;
    X509_EXTENSION *ex = NULL;
    BIO *ext_bio = NULL;
    BUF_MEM *bptr = NULL;
    char *URI = NULL;

    // Parse the certificate
    cert = d2i_X509(NULL, (const unsigned char **)&certificate->data, certificate->length);
    if (!cert) {
        *error = "unable to parse certificate in memory";
        goto on_error;
    }

    // Extract the subjectAltName extension
    int index = X509_get_ext_by_NID( cert, NID_subject_alt_name, -1);
    if (index < 0 ){
        *error = "the certificate doesn't have the subjectAltName extension";
        goto on_error;
    }
    ex = X509_get_ext(cert, index);
    if (ex == NULL){
        *error = "unable to extract subjectAltName extension";
        goto on_error;
    }

    // get the extension value
    ext_bio = BIO_new(BIO_s_mem());
    if (ext_bio == NULL){
        *error = "unable to allocate memory for extension value BIO";
        goto on_error;
    }
    if(!X509V3_EXT_print(ext_bio, ex, 0, 0)){
        *error = "unable to allocate memory for extension value BIO";
        goto on_error;
    }
    BIO_flush(ext_bio);
    BIO_get_mem_ptr(ext_bio, &bptr);

    // Find the URI in the value
    // example - URI:urn:faceplate.io:Faceplate:opcuadriver, DNS:localhost
    int URIStart = -1;
    int URIStop = -1;
    for (int i=0; i<bptr->length; i++){
        if ((URIStart == -1) && ( i+4 < bptr->length)){
            if ((bptr->data[i] == 'U') && (bptr->data[i+1] == 'R') && (bptr->data[i+2] == 'I') && (bptr->data[i+3] == ':') ){
                URIStart = i+4;
                i = i+4;
            }
        }
        if (URIStart != -1){ URIStop = i; }
        if (bptr->data[i] == ',' || bptr->data[i] == '\r' || bptr->data[i] == '\n' ){
            break;
        }
    }

    if (URIStart == -1 || URIStop == -1 || URIStart >= URIStop ){
        *error = "subjectAltName doesn'r contain URI";
        goto on_error;
    }

    // copy URI
    URI = malloc( URIStop - URIStart + 1 );
    if (URI == NULL){
        *error = "unable to allocate memory for URI";
        goto on_error;
    }
    memcpy(URI, &bptr->data[URIStart], URIStop - URIStart);
    URI[URIStop] = '\0';

    BIO_free(ext_bio);
    X509_free(cert);

    return URI;
on_error:
    if(cert != NULL){
        X509_free(cert);
    }
    if(ext_bio != NULL){
        BIO_free(ext_bio);
    }
    if(URI != NULL){
        free(URI);
    }
    return NULL;
}

cJSON* ua2json( const UA_DataType *type, void *value ){
    cJSON *result = NULL;

    if (uaIsNumber( type )){
        result = cJSON_CreateNumber( *(UA_Double*)value );
    }else if( type == &UA_TYPES[UA_TYPES_BOOLEAN]){
        result = cJSON_CreateBool( *(UA_Boolean*)value );
    }else if( type == &UA_TYPES[UA_TYPES_STRING]){
        result = cJSON_CreateString( (char *)((UA_String*)value)->data );
    }
    // TODO. Support other types
    return result;
}

UA_Variant *json2ua(const UA_DataType *type, cJSON *value){

    UA_Variant *result = UA_Variant_new();

    if (uaIsNumber( type ) && (cJSON_IsBool(value) || cJSON_IsNumber(value))){
        if ( UA_Variant_setScalarCopy( result, &value->valuedouble, type) == UA_STATUSCODE_GOOD){
            goto on_error;
        }
    }else if(type == &UA_TYPES[UA_TYPES_BOOLEAN] && (cJSON_IsBool(value) || cJSON_IsNumber(value))){
        UA_Boolean v = (value->valuedouble != 0) ;
        if ( UA_Variant_setScalarCopy( result, &v, &UA_TYPES[UA_TYPES_BOOLEAN]) == UA_STATUSCODE_GOOD) {
            goto on_error;
        }
    } else if( type == &UA_TYPES[UA_TYPES_STRING] && cJSON_IsString(value) && value->valuestring != NULL){
        UA_String  v =  UA_STRING((char *)value->valuestring);
        if (UA_Variant_setScalarCopy( result, &v, type) == UA_STATUSCODE_GOOD){
            goto on_error;
        }
    }else{
        goto on_error;
    }
    return result;

on_error:
    UA_Variant_delete(result);
    return NULL;
}

bool uaIsNumber( const UA_DataType *type ){

    if( type == &UA_TYPES[UA_TYPES_SBYTE]){
        return true;
    }else if( type == &UA_TYPES[UA_TYPES_BYTE]){
        return true;
    }else if( type == &UA_TYPES[UA_TYPES_INT16]){
        return true;
    }else if( type == &UA_TYPES[UA_TYPES_UINT16]){
        return true;
    }else if( type == &UA_TYPES[UA_TYPES_INT32]){
        return true;
    }else if( type == &UA_TYPES[UA_TYPES_UINT32]){
        return true;
    }else if( type == &UA_TYPES[UA_TYPES_INT64]){
        return true;
    }else if( type == &UA_TYPES[UA_TYPES_UINT64]){
        return true;
    }else if( type == &UA_TYPES[UA_TYPES_FLOAT]){
        return true;
    }else if( type == &UA_TYPES[UA_TYPES_DOUBLE]){
        return true;
    }

    return false;
}