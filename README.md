eopcua
=====

OPC UA driver for Erlang based on open62541 library (https://github.com/open62541/open62541).
Currently only client is supported.

Examples were written for Prosys OPC UA Simulation Server (https://www.prosysopc.com/products/opc-ua-simulation-server/)

Build
-----

    $ rebar3 compile
  
Client Example
-----
    
    {ok,Port} = eopcua_client:start_link(<<"my_connection">>).
    
    {ok,ServerList} = eopcua_client:browse_servers(Port, #{ host=> <<"localhost">>, port => 53530 } ).
    
    ok = eopcua_client:connect(Port, #{ url => hd(ServerList) }).
    
    {ok,SubItems} = eopcua_client:browse_folder(Port, <<"StaticData/AnalogItems">> ).
    
    {ok,Tree} = eopcua_client:items_tree(Port ).
    
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
    
    
Client Secured Connection
-----
    // Generate certificate
    openssl req -new -x509  -config priv/cert/example.cert.config -newkey rsa:2048 -keyout priv/eopcua.pem -nodes -outform der -out priv/eopcua.der
    
    OR
    
    {ok, #{ key := Key, certificate := Cert } } = eopcua_client:create_certificate(<<"my.connection">>).
    
    // ATTENTION! The certificate shuld be added as trusted to the OPC UA server
    
    {ok,<<"ok">>} = eopcua_client:connect(Port, #{ 
        url => <<"opc.tcp://localhost:53530/OPCUA/SimulationServer">>,
        certificate => <base64 encoded certificate in der format>,
        private_key => <base64 encoded private key in pem format>
        login => <<"test_user">>, 
        password => <<"111111">> 
    }).

Server Config 
-----
    #{
        host => <<"mynode">>,
        port => 4840,
        users => [
            #{
                login => <<"ByMeBeer">>,
                password => <<"please">>
            },
        ],
        description => #{
            productName => <<"Faceplate OPCUA Server">>,
            productUri => <<"http://faceplate.io">>,
            manufacturerName => <<"Faceplate">>,
            softwareVersion => <<"0.0.1">>,
            applicationUri => <<"urn:faceplate.io:Faceplate:OPCUA:Server">>
            
        },
        encryption => #{
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
            maxSessionTimeout => ,
            maxNodesPerRead => ,
            maxNodesPerWrite => 
        }
    }


Server Example
-----
    {ok, Port} = eopcua_server:start_link(<<"my_server">>).

    ok = eopcua_server:server_start(Port, #{
        host => <<"localhost">>,
        port => 4842,
        users => [
            #{
                login => <<"BuyMeBeer">>,
                password => <<"please">>
            }
        ],
        description => #{
            productName => <<"Faceplate OPCUA Server">>,
            productUri => <<"http://faceplate.io">>,
            manufacturerName => <<"Faceplate">>,
            softwareVersion => <<"0.0.1">>,
            applicationUri => <<"urn:faceplate.io:Faceplate:OPCUA:Server">>
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
    
    
    
