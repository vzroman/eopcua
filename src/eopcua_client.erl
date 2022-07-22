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
    set_log_level/2,
    stop/1
]).

%%==============================================================================
%%	Protocol API
%%==============================================================================
-export([
    browse_servers/2,browse_servers/3,
    connect/2,connect/3,
    read_items/2,read_items/3,
    read_item/2,read_item/3,
    write_items/2,write_items/3,
    write_item/3,write_item/4,
    browse_folder/4,browse_folder/5,
    items_tree/1,items_tree/2,
    find_recursive/3,find_recursive/4,
    create_certificate/1
]).

-define(CONNECT_TIMEOUT,30000).
-define(RESPONSE_TIMEOUT,5000).

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

set_log_level(PID, Level)->
    eport_c:set_log_level(PID, Level).
%%==============================================================================
%%	Protocol API
%%==============================================================================
browse_servers(PID, Params)->
    browse_servers(PID, Params, undefined).
browse_servers(PID, Params, Timeout)->
    eport_c:request( PID, <<"browse_servers">>, Params, Timeout ).

% Params example:
%     {
%         url => <<"opc.tcp://192.168.1.88:53530/OPCUA/SimulationServer">>,
%         ----optional---------
%         login => <<"user1">>,
%         password => <<"secret">>,
%         update_cycle => 100
%     }
connect(PID, Params)->
    connect(PID, Params,?CONNECT_TIMEOUT).
connect(PID, Params, Timeout)->
    case eport_c:request( PID, <<"connect">>, Params, Timeout ) of
        {ok, <<"ok">>} -> ok;
        Error -> Error
    end.

read_items(PID, Items)->
    read_items(PID, Items, undefined).
read_items(PID, Items, Timeout) when is_map( Items )->
    Items1 = maps:to_list( Items ),
    case read_items( PID, Items1, Timeout ) of
        {ok, Results}->
            Results1 = maps:from_list(lists:zip( Items1, Results )),
            {ok, Results1};
        Error->
            Error
    end;
read_items(PID, Items, Timeout)->
    case eport_c:request( PID, <<"read_items">>, Items, Timeout ) of
        {ok, Values}->
            Values1 = 
                [ case V of
                    <<"error: ", ItemError/binary>>->
                        {error, ItemError};
                    _-> V
                  end || V <- Values ],
            { ok, maps:from_list(lists:zip( Items, Values1 )) };
        Error->
            Error
    end.

read_item(PID, Item)->
    read_item(PID, Item, undefined).
read_item(PID, Item, Timeout)->
    eport_c:request( PID, <<"read_item">>, Item, Timeout ).

write_items(PID, Items)->
    write_items(PID, Items, undefined).
write_items(PID, Items, Timeout)->
    Items1 =
        [ [ I, V] || {I, V} <- maps:to_list( Items )],
    case eport_c:request( PID, <<"write_items">>, Items1, Timeout ) of
        {ok, Results}->
            Results1 =
                [ case V of
                      <<"error: ", ItemError/binary>>->
                          {error, ItemError};
                      _-> ok
                  end || V <- Results ],
            { ok, maps:from_list(lists:zip( [I || [I,_] <- Items1], Results1 )) };
        Error->
            Error
    end.


write_item(PID, Item, Value)->
    write_item(PID, Item, Value, undefined).
write_item(PID, Item, Value, Timeout)->
    case eport_c:request( PID, <<"write_item">>, [Item,Value], Timeout ) of
        {ok, <<"ok">>} -> ok;
        Error -> Error
    end.

browse_folder(PID, Path, Offset, Limit)->
    browse_folder(PID, Path, Offset, Limit, undefined).
browse_folder(PID, Path, Offset, Limit, Timeout)->
    Args = #{
        path => Path,
        offset => Offset,
        limit => Limit
    },
    eport_c:request( PID, <<"browse_folder">>, Args, Timeout ).

items_tree(PID)->
    items_tree(PID, undefined).
items_tree(PID, Timeout)->
    try
        {ok, items_tree(PID,Timeout,_Path = <<>>)}
    catch
        _:Error-> {error, Error}
    end.
items_tree(PID,Timeout,Path)->
    case browse_folder(PID,Path, _Offset = undefined, _Limit = undefined, Timeout) of
        {ok,Items}->
            PathPrefix =
                if
                    Path =:= <<>> -> <<>>;
                    true -> <<Path/binary,"/">>
                end,
            maps:fold(fun(Name,Item,Acc)->
                if
                    Item =:= ?FOLDER_TYPE ->
                        Acc#{Name => items_tree(PID,Timeout,<<PathPrefix/binary,Name/binary>>)};
                    Item =:= ?TAG_TYPE ->
                        Acc#{Name => Item};
                    true ->
                        % Ignore other types
                        Acc
                end
            end,#{},Items);
        {error,Error}->
            throw(Error)
    end.

find_recursive(PID, Context, Search)->
    find_recursive(PID, Context, Search, undefined).
find_recursive(PID, Context, Search, Timeout)->
    Args = #{
        context => Context,
        search => Search
    },
    eport_c:request( PID, <<"find_recursive">>, Args, Timeout ).

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



