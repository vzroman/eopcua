
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

char** str_split(char* a_str, const char a_delim){

    // Check if the string is empty
    if (strlen(a_str) == 0){
        return NULL;
    }

    a_str = strdup(a_str);
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

    free(a_str);
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

char* str_replace(const char* source, const char* search, const char* replace){
    char* result;
    int i, cnt = 0;
    int replaceLength = strlen(replace);
    int searchLength = strlen(search);
  
    // Counting the number of times old word
    // occur in the string
    for (i = 0; source[i] != '\0'; i++) {
        if (strstr(&source[i], search) == &source[i]) {
            cnt++;
  
            // Jumping to index after the old word.
            i += searchLength - 1;
        }
    }
  
    // Making new string of enough length
    result = (char*)malloc(i + cnt * (replaceLength - searchLength) + 1);
  
    i = 0;
    while (*source) {
        // compare the substring with the result
        if (strstr(source, search) == source) {
            strcpy(&result[i], replace);
            i += replaceLength;
            source += searchLength;
        }
        else
            result[i++] = *source++;
    }
  
    result[i] = '\0';
    return result;
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

char *base64_files(cJSON *files, UA_ByteString **result){
    char *error = NULL;
    UA_StatusCode sc;

    *result = NULL;
    int size = cJSON_GetArraySize( files );

    *result = (UA_ByteString *) UA_malloc(size * sizeof(UA_ByteString));
    if (!*result){
        error = "out of memory";
        goto on_error;
    }

    for (int i=0; i < size; i++){
        cJSON *file = cJSON_GetArrayItem(files, i);
        if (!cJSON_IsString(file) || (file->valuestring == NULL)){
            error = "file body is not defined";
            // It is a secure connection, the key must be provided
            goto on_error;
        }
        UA_ByteString * fileBody = parse_base64( file->valuestring );
        if (fileBody == NULL){
            error = "unable to parse file body from base64";
            goto on_error;
        }
        sc = UA_ByteString_copy(fileBody, &(*result[i]) );
        UA_ByteString_delete( fileBody );

        if (sc != UA_STATUSCODE_GOOD){
            error =  (char *)UA_StatusCode_name( sc );
            goto on_error;
        }
    }

    return error;

on_error:
    if (*result != NULL) free(*result);
    return error;

}

UA_ByteString loadFile(const char* path){
	UA_ByteString fileContents = UA_STRING_NULL;

    /* Open the file */
    FILE *fp = fopen(path, "rb");
    if(!fp) {
        errno = 0; /* We read errno also from the tcp layer... */
        return fileContents;
    }

    /* Get the file length, allocate the data and read */
    fseek(fp, 0, SEEK_END);
    fileContents.length = (size_t)ftell(fp);
    fileContents.data = (UA_Byte *)UA_malloc(fileContents.length * sizeof(UA_Byte));
    if(fileContents.data) {
        fseek(fp, 0, SEEK_SET);
        size_t read = fread(fileContents.data, sizeof(UA_Byte), fileContents.length, fp);
        if(read != fileContents.length)
            UA_ByteString_clear(&fileContents);
    } else {
        fileContents.length = 0;
    }
    fclose(fp);

    return fileContents;
}

char* parse_certificate_uri(const UA_ByteString *certificate, char **error){
    X509 *cert = NULL;
    X509_EXTENSION *ex = NULL;
    BIO *ext_bio = NULL;
    BUF_MEM *bptr = NULL;
    char *URI = NULL;

    const unsigned char * certData = (unsigned char *)certificate->data;
    size_t certLength = certificate->length;

    // Parse the certificate
    cert = d2i_X509(NULL, &certData, certLength);
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
    URI[URIStop - URIStart] = '\0';

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

    if( type == &UA_TYPES[UA_TYPES_BOOLEAN] ){
        result = cJSON_CreateBool( *(UA_Boolean*)value );
    }else if( type == &UA_TYPES[UA_TYPES_SBYTE] ){
        result = cJSON_CreateNumber( *(UA_SByte*)value );
    }else if( type == &UA_TYPES[UA_TYPES_BYTE] ){
        result = cJSON_CreateNumber( *(UA_Byte*)value );
    }else if( type == &UA_TYPES[UA_TYPES_INT16] ){
        result = cJSON_CreateNumber( *(UA_Int16*)value );
    }else if( type == &UA_TYPES[UA_TYPES_UINT16] ){
        result = cJSON_CreateNumber( *(UA_UInt16*)value );
    }else if( type == &UA_TYPES[UA_TYPES_INT32] ){
        result = cJSON_CreateNumber( *(UA_Int32*)value );
    }else if( type == &UA_TYPES[UA_TYPES_UINT32] ){
        result = cJSON_CreateNumber( *(UA_UInt32*)value );
    }else if( type == &UA_TYPES[UA_TYPES_INT64] ){
        result = cJSON_CreateNumber( *(UA_Int64*)value );
    }else if( type == &UA_TYPES[UA_TYPES_UINT64] ){
        result = cJSON_CreateNumber( *(UA_UInt64*)value );
    }else if( type == &UA_TYPES[UA_TYPES_FLOAT] ){
        result = cJSON_CreateNumber( *(UA_Float*)value );
    }else if( type == &UA_TYPES[UA_TYPES_DOUBLE] ){
        result = cJSON_CreateNumber( *(UA_Double*)value );
    }else if( type == &UA_TYPES[UA_TYPES_STRING] ){
        result = cJSON_CreateString( (char *)((UA_String*)value)->data );
    }

    // TODO. Support other types
    return result;
}

UA_Variant *json2ua(const UA_DataType *type, cJSON *value){

    UA_Variant *result = UA_Variant_new();
    bool isNumber = cJSON_IsBool(value) || cJSON_IsNumber(value);

    if (type == &UA_TYPES[UA_TYPES_SBYTE] && isNumber){
        UA_SByte v = (UA_SByte)value->valuedouble;
        if ( UA_Variant_setScalarCopy( result, &v, type) != UA_STATUSCODE_GOOD){
            goto on_error;
        }
    }else if (type == &UA_TYPES[UA_TYPES_BYTE] && isNumber){
        UA_Byte v = (UA_Byte)value->valuedouble;
        if ( UA_Variant_setScalarCopy( result, &v, type) != UA_STATUSCODE_GOOD){
            goto on_error;
        }
    }else if (type == &UA_TYPES[UA_TYPES_INT16] && isNumber){
        UA_Int16 v = (UA_Int16)value->valuedouble;
        if ( UA_Variant_setScalarCopy( result, &v, type) != UA_STATUSCODE_GOOD){
            goto on_error;
        }
    }else if (type == &UA_TYPES[UA_TYPES_UINT16] && isNumber){
        UA_UInt16 v = (UA_UInt16)value->valuedouble;
        if ( UA_Variant_setScalarCopy( result, &v, type) != UA_STATUSCODE_GOOD){
            goto on_error;
        }
    }else if (type == &UA_TYPES[UA_TYPES_INT32] && isNumber){
        UA_Int32 v = (UA_Int32)value->valuedouble;
        if ( UA_Variant_setScalarCopy( result, &v, type) != UA_STATUSCODE_GOOD){
            goto on_error;
        }
    }else if (type == &UA_TYPES[UA_TYPES_UINT32] && isNumber){
        UA_UInt32 v = (UA_UInt32)value->valuedouble;
        if ( UA_Variant_setScalarCopy( result, &v, type) != UA_STATUSCODE_GOOD){
            goto on_error;
        }
    }else if (type == &UA_TYPES[UA_TYPES_INT64] && isNumber){
        UA_Int64 v = (UA_Int64)value->valuedouble;
        if ( UA_Variant_setScalarCopy( result, &v, type) != UA_STATUSCODE_GOOD){
            goto on_error;
        }
    }else if (type == &UA_TYPES[UA_TYPES_UINT64] && isNumber){
        UA_UInt64 v = (UA_UInt64)value->valuedouble;
        if ( UA_Variant_setScalarCopy( result, &v, type) != UA_STATUSCODE_GOOD){
            goto on_error;
        }
    }else if (type == &UA_TYPES[UA_TYPES_FLOAT] && isNumber){
        UA_Float v = (UA_Float)value->valuedouble;
        if ( UA_Variant_setScalarCopy( result, &v, type) != UA_STATUSCODE_GOOD){
            goto on_error;
        }
    }else if (type == &UA_TYPES[UA_TYPES_DOUBLE] && isNumber){
        UA_Double v = (UA_Double)value->valuedouble;
        if ( UA_Variant_setScalarCopy( result, &v, type) != UA_STATUSCODE_GOOD){
            goto on_error;
        }
    }else if(type == &UA_TYPES[UA_TYPES_BOOLEAN] && isNumber){
        UA_Boolean v = (value->valueint != 0) ;
        if ( UA_Variant_setScalarCopy( result, &v, &UA_TYPES[UA_TYPES_BOOLEAN]) != UA_STATUSCODE_GOOD) {
            goto on_error;
        }
    } else if( type == &UA_TYPES[UA_TYPES_STRING] && cJSON_IsString(value) && value->valuestring != NULL){
        UA_String  v =  UA_STRING((char *)value->valuestring);
        if (UA_Variant_setScalarCopy( result, &v, type) != UA_STATUSCODE_GOOD){
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

const UA_DataType *type2ua(const char *type ){

    if( strcmp(type,"Boolean") == 0 ){
        return &UA_TYPES[UA_TYPES_BOOLEAN];
    }else if( strcmp(type,"SByte") == 0 ){
        return &UA_TYPES[UA_TYPES_SBYTE];
    }else if( strcmp(type,"Byte") == 0){
        return &UA_TYPES[UA_TYPES_BYTE];
    }else if( strcmp(type,"Int16")==0 ){
        return &UA_TYPES[UA_TYPES_INT16];
    }else if( strcmp(type,"UInt16")==0 ){
        return &UA_TYPES[UA_TYPES_UINT16];
    }else if( strcmp(type,"Int32")==0 ){
        return &UA_TYPES[UA_TYPES_INT32];
    }else if( strcmp(type,"UInt32")==0 ){
        return &UA_TYPES[UA_TYPES_UINT32];
    }else if( strcmp(type,"Int64")==0 ){
        return &UA_TYPES[UA_TYPES_INT64];
    }else if( strcmp(type,"UInt64")==0 ){
        return &UA_TYPES[UA_TYPES_UINT64];
    }else if( strcmp(type,"Float")==0 ){
        return &UA_TYPES[UA_TYPES_FLOAT];
    }else if( strcmp(type,"Double")==0 ){
        return &UA_TYPES[UA_TYPES_DOUBLE];
    }else if( strcmp(type,"String")==0 ){
        return &UA_TYPES[UA_TYPES_STRING];
    }

    // TODO. Support other types
    return NULL;
}