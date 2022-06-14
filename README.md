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
    
    {ok,EndpointList} = eopcua_client:browse_endpoints(Port, #{ host=> <<"localhost">>, port => 4840 } ).
    
    {ok,<<"ok">>} = eopcua_client:connect(Port, #{ host=> <<"localhost">>, port => 4840, endpoint => <<"OPCUA/SimulationServer">> }).
    
    {ok,SubItems} = eopcua_client:browse_folder(Port, [ <<"StaticData">>,<<"AnalogItems">> ] ).
    
    {ok,Tree} = eopcua_client:items_tree(Port ).
    
    {ok,[State,Sinusoid]} = eopcua_client:read(Port, [ <<"Server/ServerStatus/State">>, <<"Simulation/Sinusoid">> ]).
    
    {ok,[ok, {error,Error}]} = eopcua_client:write(Port, [ {<<"StaticData/AnalogItems/Int32AnalogItem">>, 34}, {<<"StaticData/AnalogItems/FloatAnalogItem">>, 34.34}]).
    
    {ok,[State,{error,Error}]} = eopcua_client:subscribe(Port, [ <<"Server/ServerStatus/State">>, <<"Simulation/Sinusoid">> ]).
    
    {ok,<<"ok">>} = eopcua_client:update_subscriptions(Port).
    
Client Secured Connection
-----
    // Generate certificate
    openssl req -new -x509  -config priv/cert/example.cert.config -newkey rsa:2048 -keyout priv/eopcua.pem -nodes -outform der -out priv/eopcua.der
    
    OR
    
    {ok, #{ key := Key, certificate := Cert } } = eopcua_client:create_certificate(<<"my.connection">>).
    
    // ATTENTION! The certificate shuld be added as trusted to the OPC UA server
    
    {ok,<<"ok">>} = eopcua_client:connect(Port, #{ 
        host=> <<"localhost">>, 
        port => 4840, 
        endpoint => <<"OPCUA/SimulationServer">>,
        certificate => <base64 encoded certificate in der format>,
        private_key => <base64 encoded private key in pem format>
        login => <<"test_user">>, 
        password => <<"111111">> 
    }).

Server Example
-----
    {ok, Port} = eopcua_server:start_link(<<"my_server">>).

    {ok, <<"ok">>} = eopcua_server:start_server(Port, #{}).

    {ok, <<"ok">>} = eopcua_server:add_nodes(Port, [
        #{ path => <<"TAGS/Folder1/tag1">>, type => integer, value => 34 },
        #{ path => <<"TAGS/Folder1/tag2">>, type => float, value => 12.34 },
        #{ path => <<"TAGS/Folder2/tag1">>, type => bool, value => true },
        #{ path => <<"TAGS/Folder2/tag2">>, type => string, value => <<"some text">> },
        #{ path => <<"TAGS/Folder2/tag2">>, type => string, value => <<"some text">> }
    ]).
    
    
