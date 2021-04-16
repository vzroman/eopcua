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
    
    {ok,<<"ok">>} = eopcua:connect(Port, #{ host=> <<"localhost">>, port => 4841, endpoint => <<"OPCUA/SimulationServer">> }).
    
    {ok,SubItems} = eopcua:browse_folder(Port, [ <<"StaticData">>,<<"AnalogItems">> ] ).
    
    {ok,Tree} = eopcua:items_tree(Port ).
    
    {ok,Value} = eopcua:read(Port, [ <<"Server">>,<<"ServerStatus">>,<<"State">> ]).
    
    {ok,<<"ok">>} = eopcua:write(Port, [ <<"StaticData">>,<<"AnalogItems">>,<<"Int32AnalogItem">> ], 34).
    
    {ok,Value} = eopcua:subscribe(Port, [ <<"StaticData">>,<<"AnalogItems">>,<<"Int32AnalogItem">> ]).
    
    {ok,<<"ok">>} = eopcua:update_subscriptions(Port).
    
    
