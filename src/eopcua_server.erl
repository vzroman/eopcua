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
-module(eopcua_server).

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
    start_server/2,start_server/3
]).

-define(CONNECT_TIMEOUT,30000).
-define(RESPONSE_TIMEOUT,5000).
-define(NO_ACTIVITY_TIMEOUT,300000). % 5 min

-define(HEADER_LENGTH,4).

%%==============================================================================
%%	Control API
%%==============================================================================
start_link(Name) ->
    start_link(Name,#{ timeout => ?CONNECT_TIMEOUT }).
start_link(Name, #{timeout := Timeout } = Options) ->
    Self = self(),
    PID = spawn_link(fun()->init( Name, Self, Options ) end),
    receive
        {PID,connected}-> {ok,PID};
        {'EXIT', PID, Reason}-> {error, Reason}
    after
        Timeout->
            stop(PID),
            { error, timeout }
    end.

stop(PID) ->
    PID ! { self(), stop }.

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
start_server(PID, Params)->
    start_server(PID, Params,?RESPONSE_TIMEOUT).
start_server(PID, Params, Timeout)->
    transaction( PID, <<"start_server">>, Params, Timeout ).


transaction( PID, Command, Body, Timeout )->
    TID = rand:uniform(16#FFFF),
    Request = #{
        <<"cmd">> => Command,
        <<"tid">> => TID,
        <<"body">> => Body
    },
    PID ! { self(), call, jsx:encode(Request), Timeout },
    wait_for_reply( PID, Command, TID, Timeout ).

wait_for_reply( PID, Command, TID, Timeout )->
    receive
        {PID, reply, {ok, Result} }-> 
            case try jsx:decode(Result, [return_maps]) catch _:_->{invalid_json, Result } end of
                #{<<"cmd">> := Command, <<"tid">> := TID, <<"reply">> := Reply}-> 
                    case Reply of
                        #{<<"type">> := <<"ok">>, <<"result">> := CmdResult}->
                            {ok, CmdResult};
                        #{<<"type">> := <<"error">>, <<"text">> := Error}->
                            {error, Error};
                        Unexpected->
                            {error, {unexpected_port_reply, Unexpected} }
                    end;
                Unexpected->
                    ?LOGWARNING("unexpected reply from the port ~p",[Unexpected]),
                    wait_for_reply( PID, Command, TID, Timeout )
            end;
        {PID, reply, Error }->
            Error
    after
        Timeout-> {error, timeout}
    end.    
%%==============================================================================
%%	Initialization procedure
%%==============================================================================
init( Name, Owner, Options ) ->
    process_flag(trap_exit, true),
    case init_ext_programm( Name ) of
        {ok,Port}->
            Owner ! {self(),connected},
            loop(Port, Owner, Options);    
        InitError->
            Owner ! InitError
    end.

init_ext_programm( Name )->
    try 
        Program = create_program_file( Name ),
        Port = open_port({spawn, Program}, [{packet, ?HEADER_LENGTH}, binary, nouse_stdio]),
        {ok,Port}
    catch
        _:Error->{error,Error}
    end.

%%==============================================================================
%%	THE LOOP
%%==============================================================================
loop( Port, Owner, Options ) ->
    receive
        {From, call, Msg, Timeout} ->
            Result = call( Port, Msg, Options, Timeout ),
            From ! {self(), reply, Result},  
            loop(Port,Owner,Options);
        {Port, {data, _Data}}->
            ?LOGWARNING("unexpected data is received from the opcua server port"),
            loop(Port, Owner, Options);
        { Owner, stop } ->
            ?LOGINFO("stopping opcua server port"),
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    ?LOGINFO("opcua server port is closed"),
                    unlink(Owner),
                    exit(normal)
            after
                30000->
                    ?LOGERROR("timeout on closing opcua server port"),
                    port_close( Port ),
                    exit( close_port_timeout )
            end;
        {'EXIT', Port, Reason} ->
            ?LOGINFO("opcua server port terminated"),
            exit({port_terminated, Reason});
        {'EXIT', Owner, Reason} ->
            ?LOGINFO("owner exit closing opcua server port"),
            port_close( Port ),
            exit( Reason );
        Unexpected->
            ?LOGWARNING("unexpected request ~p",[Unexpected]),
            loop(Port, Owner, Options)
    after
        ?NO_ACTIVITY_TIMEOUT->
            ?LOGWARNING("no activity, stop the opcua server port"),
            exit( no_activity )
    end.

call( Port, Msg, _Options, Timeout )->
    Port ! {self(), {command, Msg}},
    receive
        {Port, {data, Data}} ->
            {ok, Data }
    after
        Timeout->
            {error, timeout}
    end.

%%---------Internal helpers----------------------
create_program_file( Name )->
    Dir=code:priv_dir(eopcua),
    Source=
        case os:type() of
            {unix, linux}->atom_to_list( ?MODULE );
            {win32, _}->atom_to_list( ?MODULE ) ++ ".exe"
        end,
    SourcePath = Dir ++ "/" ++ Source,
    case filelib:is_file(SourcePath) of 
        true->
            case os:type() of
                {win32, _}->
                    % If the OS is windows we cannot launch severel instances fof the same program, to 
                    % coup with the limitation we create another copy of the file with a different unique name
                    UniquePath = Dir ++ prefix( Name ) ++ "_"++ Source,
                    case filelib:is_file(UniquePath) of
                        true-> 
                            UniquePath;
                        false->
                            case file:copy(SourcePath, UniquePath) of
                                {ok,_}->
                                    UniquePath;
                                Error->
                                    throw(Error)
                            end
                    end;
                _->
                    SourcePath
            end;
        false->
            throw({ file_not_found, SourcePath })
    end.

prefix( Name ) when is_binary(Name)->
    prefix(binary_to_list(Name));
prefix( Name ) when is_list(Name)->
    lists:append(string:replace(Name,":","_")).




