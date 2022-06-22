%%----------------------------------------------------------------
%% Copyright (c) 2022 Faceplate
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
-module(eopcua_server).

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
    server_start/2
]).


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

% Params example:
%   #{
%       host => <<"mynode">>,
%       port => 4840,
%       users => [
%           #{
%               login => <<"BuyMeBeer">>,
%               password => <<"please">>
%           }
%       ],
%   description => #{
%       productName => <<"Faceplate OPCUA Server">>,
%       productUri => <<"http://faceplate.io">>,
%       manufacturerName => <<"Faceplate">>,
%       softwareVersion => <<"0.0.1">>,
%       applicationUri => <<"urn:faceplate.io:Faceplate:OPCUA:Server">>
%   },
%   encription => #{
%       certificate => <<"base64 encoded certificate in der format">>,
%       private_key => <<"base64 encoded private key in pem format">>,
%       trustList => [
%           <<"base64 encoded certificate in der format">>
%       ],
%   },
%   limits => #{
%       maxSecureChannels => ,
%       maxSecurityTokenLifetime => , % in ms
%       maxSessions => ,
%       maxSessionTimeout => ,
%       maxNodesPerRead => ,
%       maxNodesPerWrite =>
%   }
%}
server_start(PID, Params)->
    case eport_c:request( PID, <<"server_start">>, Params ) of
        {ok, <<"ok">>} -> ok;
        Error -> Error
    end.




