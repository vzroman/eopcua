# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)]).")
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)]).")
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)]).")

OPEN62541_INCLUDE_DIR ?= $(CURDIR)/open62541/build/_install/include
OPEN62541_LIB64_DIR ?= $(CURDIR)/open62541/build/_install/lib64
OPEN62541_LIB_DIR ?= $(CURDIR)/open62541/build/_install/lib

CJSON_INCLUDE_DIR ?= $(CURDIR)/cJSON/build/_install/include
CJSON_LIB64_DIR ?= $(CURDIR)/cJSON/build/_install/lib64
CJSON_LIB_DIR ?= $(CURDIR)/cJSON/build/_install/lib

UTHASH_INCLUDE_DIR ?= $(CURDIR)/uthash/include

EPORT_INCLUDE_DIR ?= $(CURDIR)/eport/include

INCLUDE_DIR = -I $(ERL_INTERFACE_INCLUDE_DIR) -I $(OPEN62541_INCLUDE_DIR) -I $(CJSON_INCLUDE_DIR) -I $(UTHASH_INCLUDE_DIR) -I $(EPORT_INCLUDE_DIR)
CLIENT_INCLUDE_DIR ?= -I $(CURDIR)/client/include
SERVER_INCLUDE_DIR ?= -I $(CURDIR)/server/include

C_SRC_DIR = $(CURDIR)
C_SRC_OUTPUT_CLIENT ?= $(CURDIR)/../priv/$(PROJECT)_client
C_SRC_OUTPUT_SERVER ?= $(CURDIR)/../priv/$(PROJECT)_server

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -arch x86_64 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -arch x86_64 -finline-functions -Wall
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
endif

CFLAGS += -fPIC $(INCLUDE_DIR)
CXXFLAGS += -fPIC $(INCLUDE_DIR)

CLIENT_CFLAGS = $(CFLAGS) $(CLIENT_INCLUDE_DIR)
CLIENT_CXXFLAGS += $(CXXFLAGS) $(CLIENT_INCLUDE_DIR)

SERVER_CFLAGS = $(CFLAGS) $(SERVER_INCLUDE_DIR)
SERVER_CXXFLAGS += $(CXXFLAGS) $(SERVER_INCLUDE_DIR)

LDLIBS += -lssl -lcrypto -lpthread -L $(ERL_INTERFACE_LIB_DIR) -lerl_interface -lei -L $(OPEN62541_LIB_DIR) -L $(OPEN62541_LIB64_DIR) -lopen62541 -L $(CJSON_LIB_DIR) -L $(CJSON_LIB64_DIR) -lcjson -lcjson_utils


# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

CLIENT_SOURCES := $(shell find $(C_SRC_DIR) -type d \( -name "open62541" -o -name "cJSON" -o -name "uthash" -o -name "server" \) -prune -false -o -type f \( -name "*.c" -o -name "*.C" -o -name "*.cc" -o -name "*.cpp" \))
CLIENT_OBJECTS = $(addsuffix .o, $(basename $(CLIENT_SOURCES)))

SERVER_SOURCES := $(shell find $(C_SRC_DIR) -type d \( -name "open62541" -o -name "cJSON" -o -name "uthash" -o -name "client" \) -prune -false -o -type f \( -name "*.c" -o -name "*.C" -o -name "*.cc" -o -name "*.cpp" \))
SERVER_OBJECTS = $(addsuffix .o, $(basename $(SERVER_SOURCES)))

$(C_SRC_OUTPUT_CLIENT) : COMPILE_C = $(c_verbose) $(CC) $(CLIENT_CFLAGS) $(CPPFLAGS) -c
$(C_SRC_OUTPUT_CLIENT) : COMPILE_CPP = $(cpp_verbose) $(CXX) $(CLIENT_CXXFLAGS) $(CPPFLAGS) -c

$(C_SRC_OUTPUT_SERVER) : COMPILE_C = $(c_verbose) $(CC) $(SERVER_CFLAGS) $(CPPFLAGS) -c
$(C_SRC_OUTPUT_SERVER) : COMPILE_CPP = $(cpp_verbose) $(CXX) $(SERVER_CXXFLAGS) $(CPPFLAGS) -c

all: $(C_SRC_OUTPUT_CLIENT) $(C_SRC_OUTPUT_SERVER)


$(C_SRC_OUTPUT_CLIENT): $(CLIENT_OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) $(CLIENT_OBJECTS) $(LDFLAGS) $(LDLIBS) -o $(C_SRC_OUTPUT_CLIENT)

$(C_SRC_OUTPUT_SERVER): $(SERVER_OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) $(SERVER_OBJECTS) $(LDFLAGS) $(LDLIBS) -o $(C_SRC_OUTPUT_SERVER)

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<

%.o: %.cc
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.C
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.cpp
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

clean:
	@rm -f $(C_SRC_OUTPUT_CLIENT) $(C_SRC_OUTPUT_SERVER) $(CLIENT_OBJECTS) $(SERVER_OBJECTS)
