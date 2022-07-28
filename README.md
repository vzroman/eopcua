eopcua
=====

OPC UA driver for Erlang based on open62541 library (https://github.com/open62541/open62541).
Server and client is supported. Currently, the functionality is limited to reading and writing simple (scalar) types:
    * Boolean
    * SByte
    * Byte
    * Int16
    * UInt16
    * Int32
    * UInt32
    * Int64
    * UInt64
    * Float
    * Double
    * String

I tried to keep API is as simple as possible.

Examples were written for Prosys OPC UA Simulation Server (https://www.prosysopc.com/products/opc-ua-simulation-server/)

I appreciate any pull requests for bug fixing or extending the functionality. 

Build
-----

    $ rebar3 compile
  
Client Example
-----
    
    {ok,Port} = eopcua_client:start_link(<<"my_connection">>).
    
    {ok,ServerList} = eopcua_client:browse_servers(Port, #{ host=> <<"localhost">>, port => 53530 } ).
    
    ok = eopcua_client:connect(Port, #{ url => hd(ServerList), max_nodes_per_browse => 1000 }).

    % ResultMap has format:
    %   #{
    %       Path:=NodeClass
    %       ...
    %   }
    % Search by empty string returns all the items
    {ok, ResultMap} = eopcua_client:search(Port, <<"Analog">> ).
    
    {ok,SinusoidValue} = eopcua_client:read_item(Port, <<"Simulation/Sinusoid">> ).

    {ok,#{
        <<"Server/ServerStatus/State">> := State,
        <<"Simulation/Sinusoid">> := Sinusoid
    }} = eopcua_client:read_items(Port, [ <<"Server/ServerStatus/State">>, <<"Simulation/Sinusoid">> ]).
    
    ok = eopcua_client:write_item(Port, <<"StaticData/AnalogItems/Int32AnalogItem">>, 38).

    {ok,#{
        <<"StaticData/AnalogItems/Int32AnalogItem">> := ok,
        <<"StaticData/AnalogItems/ItDoesnNotExist">> := {error, Error}
    }} = eopcua_client:write_items(Port, #{<<"StaticData/AnalogItems/Int32AnalogItem">> => 34, <<"StaticData/AnalogItems/ItDoesnNotExist">> => 34.34}).

    ok = eopcua_client:set_log_level(Port, trace).  #; trace, debug, info, warning, error, fatal

    eopcua_client:stop(Port).
    
    
Client Encrypted Connection
-----
    // Generate certificate
    openssl req -new -x509  -config priv/cert/example.cert.config -newkey rsa:2048 -keyout priv/eopcua.pem -nodes -outform der -out priv/eopcua.der
    
    OR
    
    {ok, #{ key := Key, certificate := Cert } } = eopcua_client:create_certificate(<<"my.connection">>).

    {ok, Port} = eopcua_client:start_link(<<"my_connection">>).
    
    // ATTENTION! The certificate shuld be added as trusted to the OPC UA server
    
    ok = eopcua_client:connect(Port, #{ 
        url => <<"opc.tcp://localhost:53530/OPCUA/SimulationServer">>,
        certificate => base64:encode(Cert),     % Certificated in der format
        private_key => base64:encode(Key),      % Private key in pem format
        login => <<"test_user">>, 
        password => <<"111111">>,
        max_nodes_per_browse => 1000
    }).

Server Config 
-----
    #{
        host => <<"mynode">>,
        port => 4840,
        access => #{
            enable_anonymous => false,
            users => [
                #{
                    login => <<"BuyMeBeer">>,
                    password => <<"please">>
                },
            ]
        },
        description => #{
            productName => <<"My OPCUA Server">>,
            productUri => <<"http://mysite.com">>,
            manufacturerName => <<"Me">>,
            softwareVersion => <<"0.0.1">>,
            applicationUri => <<"urn:mysite.com:MyApplication:OPCUA:Server">>
            
        },
        encryption => #{
            enable_unencrypted => false,
            certificate => <<"base64 encoded certificate in der format">>,
            private_key => <<"base64 encoded private key in pem format">>,
            trustList => [
                <<"base64 encoded certificate in der format">>
            ],
            issuerList => [
                <<"base64 encoded certificate in der format">>
            ],
            revocationList => [
                <<"base64 encoded certificate in der format">>
            ]
        },
        limits => #{
            maxSecureChannels => ,
            maxSecurityTokenLifetime => , % in ms
            maxSessions => ,
            maxSessionTimeout => , % in ms
            maxNodesPerRead => ,
            maxNodesPerWrite => 
        }
    }


Server Example
-----
    {ok, Port} = eopcua_server:start_link(<<"my_server">>).

    ok = eopcua_server:server_start(Port, #{
        host => <<"localhost">>,
        port => 4841,
        access => #{
            enable_anonymous => true,
            users => [
                #{
                    login => <<"BuyMeBeer">>,
                    password => <<"please">>
                }
            ]
        },
        description => #{
            productName => <<"My OPCUA Server">>,
            productUri => <<"http://mysite.com">>,
            manufacturerName => <<"Me">>,
            softwareVersion => <<"0.0.1">>,
            applicationUri => <<"urn:mysite.com:MyApplication:OPCUA:Server">>
        }
    }).
    
    ok = eopcua_server:write_item(Port, #{path => <<"TAGS/my_folder/temperature">>, type => <<"Double">>, value => 45.67}).

    {ok, #{
        <<"TAGS/my_folder/temperature">> := ok,
        <<"TAGS/my_folder/pressure">> := ok         % or it can be {error, Error}
    }} = eopcua_server:write_items(Port, [
        #{path => <<"TAGS/my_folder/temperature">>, type => <<"Double">>, value => 34.56},
        #{path => <<"TAGS/my_folder/pressure">>, type => <<"UInt32">>, value => 87}
    ]).

    {ok, Temperature} = eopcua_server:read_item(Port, <<"TAGS/my_folder/temperature">>).

    {ok, #{
        <<"TAGS/my_folder/temperature">> := Temperature,
        <<"TAGS/my_folder/pressure">> := Pressure         % or it can be {error, Error}
    }} = eopcua_server:read_items(Port, [
        <<"TAGS/my_folder/temperature">>,
        <<"TAGS/my_folder/pressure">>
    ]).

    ok = eopcua_server:set_log_level(Port, debug).  #; trace, debug, info, warning, error, fatal

Server Encrypted Connection
-----
    // Generate certificate
    {ok, #{ key := Key, certificate := Cert } } = eopcua_server:create_certificate(<<"my.server">>).

    {ok, Port} = eopcua_server:start_link(<<"my_server_with_encryption">>).
    
    ok = eopcua_server:server_start(Port, #{
        host => <<"localhost">>,
        port => 4842,
        access => #{
            enable_anonymous => false,
            users => [
                #{
                    login => <<"BuyMeBeer">>,
                    password => <<"please">>
                }
            ]
        },
        encryption => #{
            enable_unencrypted => false,
            certificate => base64:encode(Cert),     % Certificated in der format
            private_key => base64:encode(Key)       % Private key in pem format
        },
        description => #{
            productName => <<"My OPCUA Server">>,
            productUri => <<"http://mysite.com">>,
            manufacturerName => <<"Me">>,
            softwareVersion => <<"0.0.1">>
        }
    }).
    
    
