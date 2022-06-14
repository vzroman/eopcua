%%----------------------------------------------------------------
%% Copyright (c) 2021 Faceplate
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%----------------------------------------------------------------
-module(eopcua_client).

-include("eopcua.hrl").

%%==============================================================================
%%	Control API
%%==============================================================================
-export([
    start_link/1, start_link/2,
    stop/1
]).

%%==============================================================================
%%	Protocol API
%%==============================================================================
-export([
    connect/2,connect/3,
    read/2,read/3,
    write/2,write/3,
    subscribe/2,subscribe/3,
    update_subscriptions/1,update_subscriptions/2,
    browse_endpoints/2,browse_endpoints/3,
    browse_folder/2,browse_folder/3,
    items_tree/1,items_tree/2,
    create_certificate/1
]).

-export([
    test/0
]).

-define(CONNECT_TIMEOUT,30000).
-define(RESPONSE_TIMEOUT,5000).
-define(NO_ACTIVITY_TIMEOUT,300000). % 5 min

-define(FOLDER_TYPE,1).
-define(TAG_TYPE,2).

-define(HEADER_LENGTH,4).

%%==============================================================================
%%	Control API
%%==============================================================================
start_link(Name) ->
    start_link(Name,#{ response_timeout => ?RESPONSE_TIMEOUT }).
start_link(Name, Options) ->
    Dir=code:priv_dir(eopcua),
    Source=
        case os:type() of
            {unix, linux}->atom_to_list( ?MODULE );
            {win32, _}->atom_to_list( ?MODULE ) ++ ".exe"
        end,
    SourcePath = unicode:characters_to_binary(Dir ++ "/" ++ Source),
    eport_c:start_link(SourcePath, Name, Options).

stop(PID) ->
    eport_c:stop( PID ).

%%==============================================================================
%%	Protocol API
%%==============================================================================
% Params example:
%     {
%         host => <<"localhost">>,
%         port => 4841,
%         ----optional---------
%         endpoint => <<"OPCUA/SimulationServer">>,
%         login => <<"user1">>,
%         password => <<"secret">>
%     }
connect(PID, Params)->
    connect(PID, Params,?CONNECT_TIMEOUT).
connect(PID, Params, Timeout)->
    eport_c:request( PID, <<"connect">>, Params, Timeout ).   

read(PID, Items)->
    read(PID,Items,?RESPONSE_TIMEOUT).
read(PID, Items, Timeout) when is_map( Items )->
    Items1 = maps:to_list( Items ),
    case read( PID, Items1, Timeout ) of
        {ok, Results}->
            Results1 = maps:from_list(lists:zip( Items1, Results )),
            {ok, Results1};
        Error->
            Error
    end;    
read(PID, Items, Timeout)->
    Items1 = 
        [ binary:split(I, <<"/">>, [global] ) || I <- Items],
    case eport_c:request( PID, <<"read">>, Items1, Timeout ) of
        {ok, Values}->
            Values1 = 
                [ case V of
                    <<"error: ", ItemError/binary>>->
                        {error, ItemError};
                    _-> V
                  end || V <- Values ],
            { ok, Values1 };   
        Error->
            Error
    end.

write(PID, Items)->
    write(PID,Items,?RESPONSE_TIMEOUT).
write(PID, Items, Timeout) when is_map( Items )->
    Items1 = maps:to_list( Items ),
    case write(PID, Items1, Timeout) of
        {ok, Results}->
            Results1 = maps:from_list(lists:zip( Items1, Results )),
            { ok, Results1 };
        Error->
            Error
    end;
write(PID, Items, Timeout)->
    Items1 = 
        [ [ binary:split(I, <<"/">>, [global] ), V] || {I, V} <- Items],
    case eport_c:request( PID, <<"write">>, Items1, Timeout ) of
        {ok, Results}->
            update_subscriptions(PID),
            Results1 = 
                [ case V of
                    <<"error: ", ItemError/binary>>->
                        {error, ItemError};
                    _-> ok
                  end || V <- Results ],
            { ok, Results1 };   
        Error->
            Error
    end.    

subscribe(PID, Items)->
    subscribe(PID,Items,?RESPONSE_TIMEOUT).
subscribe(PID, Items, Timeout) when is_map(Items)->
    Items1 = maps:to_list( Items ),
    case subscribe( PID, Items1, Timeout ) of
        {ok, Results}->
            Results1 = maps:from_list(lists:zip( Items1, Results )),
            {ok, Results1};
        Error->
            Error
    end;
subscribe(PID, Items, Timeout)->
    Items1 = 
        [ binary:split(I, <<"/">>, [global] ) || I <- Items],
    case eport_c:request( PID, <<"subscribe">>, Items1, Timeout ) of
        {ok, Values}->
            Values1 = 
                [ case V of
                    <<"error: ", ItemError/binary>>->
                        {error, ItemError};
                    _-> V
                  end || V <- Values ],
            { ok, Values1 };   
        Error->
            Error
    end.    

update_subscriptions(PID)->
    update_subscriptions(PID,?RESPONSE_TIMEOUT).
update_subscriptions(PID, Timeout)->
    eport_c:request( PID, <<"update_subscriptions">>, <<"true">>, Timeout ).

browse_endpoints(PID, Params)->
    browse_endpoints(PID, Params,?RESPONSE_TIMEOUT).
browse_endpoints(PID, Params, Timeout)->
    eport_c:request( PID, <<"browse_endpoints">>, Params, Timeout ).  

browse_folder(PID, Path)->
    browse_folder(PID, Path, ?RESPONSE_TIMEOUT).
browse_folder(PID, Path, Timeout)->
    eport_c:request( PID, <<"browse_folder">>, Path, Timeout ).  

items_tree(PID)->
    items_tree(PID, ?RESPONSE_TIMEOUT).
items_tree(PID, Timeout)->
    try
        {ok, items_tree(PID,Timeout,_Path = [])}
    catch
        _:Error-> {error, Error}
    end.
items_tree(PID,Timeout,Path)->
    case browse_folder(PID,Path,Timeout) of
        {ok,Items}->
            maps:fold(fun(Name,Item,Acc)->
                case Item of
                    #{<<"type">> := ?FOLDER_TYPE,<<"id">>:=ID}->
                        Acc#{Name => #{<<"id">>=>ID, <<"children">>=> items_tree(PID,Timeout,Path ++ [Name])}};
                    #{<<"type">> := ?TAG_TYPE,<<"id">>:=ID}->
                        Acc#{Name => ID};
                    _->
                        % Ignore other types
                        Acc
                end    
            end,#{},Items);
        {error,Error}->
            throw(Error)
    end.   

create_certificate( Name )->
    Priv = code:priv_dir(eopcua),
    Key = Priv++"/eopcua.pem",
    Cert = Priv++"/eopcua.der",

    Cmd = 
        "openssl req -new -x509  -config "++
        Priv++"/cert/example.cert.config -newkey rsa:2048 -keyout "++
        Key++" -nodes -outform der "++
        "-subj '/CN="++unicode:characters_to_list(Name)++"' "++
        "-out "++Cert,
    
    Out = os:cmd( Cmd ),

    Result =
        case { file:read_file(Key), file:read_file(Cert) } of
            { {ok, KeyData}, {ok, CertData} }->
                {ok, #{ key => KeyData, certificate => CertData } };
            _->
                {error, { Cmd, Out }}
        end,
    file:delete(Key),
    file:delete(Cert),

    Result.

test()->
    {ok,Port} = eopcua_client:start_link(<<"my_connection">>),

    {ok, Cert} = file:read_file("/home/roman/PROJECTS/SOURCES/eopcua/cert/eopcua.der"),
    {ok, Key} = file:read_file("/home/roman/PROJECTS/SOURCES/eopcua/cert/eopcua.pem"),

    {ok,<<"ok">>} = eopcua_client:connect(Port, #{
        host=> <<"localhost">>, 
        port => 53530, endpoint => <<"OPCUA/SimulationServer">>, 
        login => <<"test_user">>, 
        password => <<"111111">>, 
        certificate=> base64:encode(Cert), 
        private_key=> base64:encode( Key)
    }).



