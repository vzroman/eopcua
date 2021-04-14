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
-module(eopcua).

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
    write/3,write/4,
    subscribe/2,subscribe/3,
    update_subscriptions/1,update_subscriptions/2,
    browse_endpoints/2,browse_endpoints/3,
    browse_folder/2,browse_folder/3
]).

-define(CONNECT_TIMEOUT,30000).
-define(RESPONSE_TIMEOUT,5000).

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
connect(PID, Params)->
    connect(PID, Params,?RESPONSE_TIMEOUT).
connect(PID, Params, Timeout)->
    transaction( PID, <<"connect">>, Params, Timeout ).   

read(PID, Path)->
    read(PID,Path,?RESPONSE_TIMEOUT).
read(PID, Path, Timeout)->
    transaction( PID, <<"read">>, Path, Timeout ).

write(PID, Path, Value)->
    write(PID,Path, Value,?RESPONSE_TIMEOUT).
write(PID, Path, Value, Timeout)->
    transaction( PID, <<"write">>, #{ <<"tag">> => Path, <<"value">> => Value}, Timeout ).

subscribe(PID, Path)->
    subscribe(PID,Path,?RESPONSE_TIMEOUT).
subscribe(PID, Path, Timeout)->
    transaction( PID, <<"subscribe">>, Path, Timeout ).

update_subscriptions(PID)->
    update_subscriptions(PID,?RESPONSE_TIMEOUT).
update_subscriptions(PID, Timeout)->
    transaction( PID, <<"update_subscriptions">>, <<"true">>, Timeout ).

browse_endpoints(PID, Params)->
    browse_endpoints(PID, Params,?RESPONSE_TIMEOUT).
browse_endpoints(PID, Params, Timeout)->
    transaction( PID, <<"browse_endpoints">>, Params, Timeout ).  

browse_folder(PID, Path)->
    browse_folder(PID, Path, ?RESPONSE_TIMEOUT).
browse_folder(PID, Path, Timeout)->
    transaction( PID, <<"browse_folder">>, Path, Timeout ).  

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
            ?LOGWARNING("unexpected data is received from the port"),
            loop(Port, Owner, Options);
        { Owner, stop } ->
            Port ! {self(), close},
            receive
                {Port, closed} -> exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            exit({port_terminated, Reason});
        {'EXIT', Owner, Reason} ->
            port_close( Port ),
            exit( Reason );
        _Unexpected->
            % TODO. Log it
            loop(Port, Owner, Options)
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
    Dir=code:priv_dir(?MODULE),
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


