eopcua
=====

An OTP library

Build
-----

    $ rebar3 compile
  
Example
-----
    
    {ok,Port} = eopcua:start_link(<<"my_connection">>).
    
    {ok,EndpointList} = eopcua:browse_endpoints(Port, #{ host=> <<"localhost">>, port => 4841 } ).
    
    {ok,SubItems} = eopcua:browse_folder(Port, [ <<"StaticData">>,<<"AnalogItems">> ] ).
    
    {ok,<<"ok">>} = eopcua:connect(Port, #{ host=> <<"localhost">>, port => 4841, endpoint => <<"OPCUA/SimulationServer">> }).
    
    {ok,Value} = eopcua:read(Port, [ <<"Server">>,<<"ServerStatus">>,<<"State">> ]).
    
    {ok,<<"ok">>} = eopcua:write(Port, [ <<"StaticData">>,<<"AnalogItems">>,<<"Int32AnalogItem">> ], 34).
    
    
