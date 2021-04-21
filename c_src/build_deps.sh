#!/bin/sh
# based on build_deps.sh from basho/eleveldb

open62541_VSN="v1.2"

set -e

# the script folder
DIR=$PWD
BASEDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

# dive into c_src
cd $BASEDIR

# detecting gmake and if exists use it
# if not use make
# (code from github.com/tuncer/re2/c_src/build_deps.sh
which gmake 1>/dev/null 2>/dev/null && MAKE=gmake
MAKE=${MAKE:-make}

case "$1" in
    clean)
        rm -rf open62541
        rm -rf cJSON
        ;;

    build)
        # open62541
        if [ ! -d open62541 ]; then
            git clone -b $open62541_VSN https://github.com/open62541/open62541.git
        fi
        cd open62541
        mkdir -p build
        cd build
        cmake -DCMAKE_INSTALL_PREFIX=_install -DUA_ENABLE_ENCRYPTION=On -DUA_ENABLE_ENCRYPTION_OPENSSL=On -DUA_BUILD_SELFSIGNED_CERTIFICATE=On .. 
        make && make install

        # cJSON
        cd $BASEDIR
        if [ ! -d cJSON ]; then
            git clone -b v1.7.14 https://github.com/DaveGamble/cJSON.git
        fi
        cd cJSON
        mkdir -p build
        cd build
        cmake cmake .. -DENABLE_CJSON_UTILS=On -DENABLE_CJSON_TEST=Off -DCMAKE_INSTALL_PREFIX=_install -DBUILD_SHARED_LIBS=Off 
        make && make install

        # uthash
        cd $BASEDIR
        if [ ! -d uthash ]; then
            git clone -b v2.3.0 https://github.com/troydhanson/uthash.git
        fi
        
        ;;
esac

cd $DIR
