eopcua
=====

OPC UA driver for Erlang based on open62541 library (https://github.com/open62541/open62541).
Currently only client is supported.

Examples were written for Prosys OPC UA Simulation Server (https://www.prosysopc.com/products/opc-ua-simulation-server/)

Build
-----

    $ rebar3 compile
  
Example
-----
    
    {ok,Port} = eopcua:start_link(<<"my_connection">>).
    
    {ok,EndpointList} = eopcua:browse_endpoints(Port, #{ host=> <<"localhost">>, port => 4840 } ).
    
    {ok,<<"ok">>} = eopcua:connect(Port, #{ host=> <<"localhost">>, port => 4840, endpoint => <<"OPCUA/SimulationServer">> }).
    
    {ok,SubItems} = eopcua:browse_folder(Port, [ <<"StaticData">>,<<"AnalogItems">> ] ).
    
    {ok,Tree} = eopcua:items_tree(Port ).
    
    {ok,[State,Sinusoid]} = eopcua:read(Port, [ <<"Server/ServerStatus/State">>, <<"Simulation/Sinusoid">> ]).
    
    {ok,[ok, {error,Error}]} = eopcua:write(Port, [ {<<"StaticData/AnalogItems/Int32AnalogItem">>, 34}, {<<"StaticData/AnalogItems/FloatAnalogItem">>, 34.34}]).
    
    {ok,[State,{error,Error}]} = eopcua:subscribe(Port, [ <<"Server/ServerStatus/State">>, <<"Simulation/Sinusoid">> ]).
    
    {ok,<<"ok">>} = eopcua:update_subscriptions(Port).
    
Secured connection
-----
    // Generate certificate
    openssl req -new -x509  -config cert/example.cert.config -newkey rsa:2048 -keyout cert/eopcua.pem -nodes -outform der -out cert/eopcua.der
    
    // ATTENTION! The certificate shuld be added as trusted to the OPC UA server
    
    {ok,<<"ok">>} = eopcua:connect(Port, #{ 
        host=> <<"localhost">>, 
        port => 4840, 
        endpoint => <<"OPCUA/SimulationServer">>,
        certificate => <base64 encoded certificate in der format>,
        private_key => <base64 encoded private key in pem format>
        login => <<"test_user">>, 
        password => <<"111111">> 
    }).
    
    
    
