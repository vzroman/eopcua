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
    case transaction( PID, <<"read">>, Items1, Timeout ) of
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
    case transaction( PID, <<"write">>, Items1, Timeout ) of
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
    case transaction( PID, <<"subscribe">>, Items1, Timeout ) of
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
    transaction( PID, <<"update_subscriptions">>, <<"true">>, Timeout ).

browse_endpoints(PID, Params)->
    browse_endpoints(PID, Params,?RESPONSE_TIMEOUT).
browse_endpoints(PID, Params, Timeout)->
    transaction( PID, <<"browse_endpoints">>, Params, Timeout ).  

browse_folder(PID, Path)->
    browse_folder(PID, Path, ?RESPONSE_TIMEOUT).
browse_folder(PID, Path, Timeout)->
    transaction( PID, <<"browse_folder">>, Path, Timeout ).  

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
            ?LOGINFO("stopping port"),
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    ?LOGINFO("port is closed"),
                    unlink(Owner),
                    exit(normal)
            after
                30000->
                    ?LOGERROR("timeout on closing opcua port"),
                    port_close( Port ),
                    exit( close_port_timeout )
            end;
        {'EXIT', Port, Reason} ->
            ?LOGINFO("port terminated"),
            exit({port_terminated, Reason});
        {'EXIT', Owner, Reason} ->
            ?LOGINFO("owner exit closing port"),
            port_close( Port ),
            exit( Reason );
        Unexpected->
            ?LOGWARNING("unexpected request ~p",[Unexpected]),
            loop(Port, Owner, Options)
    after
        ?NO_ACTIVITY_TIMEOUT->
            ?LOGWARNING("no activity, stop the port"),
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



test()->
    {ok,Port} = eopcua:start_link(<<"my_connection">>),

    {ok, Cert} = file:read_file("/home/roman/PROJECTS/SOURCE/OPCUA/eopcua/cert/eopcua.der"),
    {ok, Key} = file:read_file("/home/roman/PROJECTS/SOURCE/OPCUA/eopcua/cert/eopcua.pem"),

    {ok,<<"ok">>} = eopcua:connect(Port, #{ 
        host=> <<"localhost">>, 
        port => 4840, endpoint => <<"OPCUA/SimulationServer">>, 
        login => <<"test_user">>, 
        password => <<"111111">>, 
        certificate=> base64:encode(Cert), 
        private_key=> base64:encode( Key)
    }).



