-module(eopcua).

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
    read_value/2
]).

-define(CONNECT_TIMEOUT,30000).
-define(RESPONSE_TIMEOUT,5000).

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
read_value(PID, Node)->
    read_value(PID,Node,?RESPONSE_TIMEOUT).
read_value(PID, Node, Timeout)->
    call_ext( PID, {read_value, Node}, Timeout ).

call_ext( PID, Msg, Timeout )->
    PID ! { self(), call, Msg, Timeout },
    receive
        {PID, reply, Result }-> Result
    after
        Timeout-> {error,timeout}
    end.    
%%==============================================================================
%%	Initialization procedure
%%==============================================================================
init( Name, Owner, Options ) ->
    process_flag(trap_exit, true),
    case init_ext_programm( Name ) of
        {ok,Port}->
            case connect(Port, Name, Options) of
                ok->
                    Owner ! {self(),connected},
                    loop(Port, Owner, Options);
                ConnectError->
                    port_close(Port),
                    Owner ! ConnectError
            end;    
        InitError->
            Owner ! InitError
    end.

init_ext_programm( Name )->
    try 
        Program = create_program_file( Name ),
        Port = open_port({spawn, Program}, [{packet, 2}, binary, nouse_stdio]),
        {ok,Port}
    catch
        _:Error->{error,Error}
    end.

connect(Port, Name, #{timeout := Timeout})->
    Port ! {self(), {command, encode({connect,Name})}},
    receive
		{Port, {data, Data}} ->
		    case decode(Data) of
                connected-> ok;
                Error-> Error
            end;
        {'EXIT', Port, Reason} ->
	        {error,{port_terminated,Reason}}
    after
        Timeout ->
            {error,connect_timeout}

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
        {Port, {data, Data}}->
            notify( Owner, Data, Options ),
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
    Port ! {self(), {command, encode(Msg)}},
    receive
        {Port, {data, Data}} ->
            {ok, decode(Data) }
    after
        Timeout-> {error, timeout}
    end.

notify( Owner, Data, _Options )->
    case decode( Data ) of
        { notify, Msg }-> Owner ! { notify, Msg };
        _Unexpected->
            % TODO. Log it
            ignore
    end.


encode(Message)->
    term_to_binary(Message).
decode(Message)->
    binary_to_term(Message).

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


