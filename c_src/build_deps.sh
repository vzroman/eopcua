#!/bin/sh
# based on build_deps.sh from basho/eleveldb

open62541_VSN="v1.3.6"

set -e

# the script folder
DIR=$PWD
BASEDIR="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

# dive into c_src
cd $BASEDIR

# detecting gmake and if exists use it
# if not use make
# (code from github.com/tuncer/re2/c_src/build_deps.sh
which gmake 1>/dev/null 2>/dev/null && MAKE=gmake
MAKE=${MAKE:-make}

# This is a workaround of compilation error of cJSON: 
#   generic selections are a C11-specific feature
TARGET_OS=`uname -s`
if [ "$TARGET_OS" = "Darwin" ] || [ "$TARGET_OS" = "FreeBSD" ]; then
    export CFLAGS="$CFLAGS -Wno-c11-extensions"
fi

case "$1" in
    clean)
        rm -rf open62541
        rm -rf uthash
        ;;

    build)
        # open62541
        if [ ! -d open62541 ]; then
            git clone --depth 1 -b $open62541_VSN https://github.com/open62541/open62541.git
        fi
        cd open62541
        mkdir -p build
        cd build
        cmake -DCMAKE_INSTALL_PREFIX=_install -DUA_ENABLE_ENCRYPTION=On -DUA_ENABLE_ENCRYPTION_OPENSSL=On -DUA_BUILD_SELFSIGNED_CERTIFICATE=On .. 
        make && make install

        # uthash
        cd $BASEDIR
        if [ ! -d uthash ]; then
            git clone --depth 1 -b v2.3.0 https://github.com/troydhanson/uthash.git
        fi
        
        ;;
esac

cd $DIR
