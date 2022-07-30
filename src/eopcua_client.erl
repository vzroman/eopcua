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
    write_items/2,write_items/3,
    search/2,search/3,
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
    Host =
        case Params of
            #{host:=H} ->H;
            #{<<"host">>:=H}->H
        end,
    case eport_c:request( PID, <<"browse_servers">>, Params, Timeout ) of
        {ok, Endpoints} when length(Endpoints)>0->
            {ok, [ replace_host(E, Host) || E <- Endpoints]};
        {ok,_}->
            {error, no_endpoints_found};
        Error->
            Error
    end.

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
read_items(PID, Items, Timeout)->
    eport_c:request( PID, <<"read_items">>, Items, Timeout ).

write_items(PID, Items)->
    write_items(PID, Items, undefined).
write_items(PID, Items, Timeout)->
    eport_c:request( PID, <<"write_items">>, Items, Timeout ).


search(PID, Search)->
    search(PID, Search, undefined).
search(PID, Search, Timeout)->
    eport_c:request( PID, <<"search">>, Search, Timeout ).

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

replace_host(Endpoint, Host)->
    % Open62541 sometimes returns bad strings in ad[i].discoveryUrls[j].data
    % probably without the null at the end
    E = << <<C>> || <<C>> <= Endpoint, C >= 33, C =< 127 >>,
    % We need to replace host in the original endpoint to be able to connect
    % to the server even if its host name is not resolved to the IP
    re:replace(E,"//.*:",<<"//",Host/binary,":">>,[{return,binary}]).


