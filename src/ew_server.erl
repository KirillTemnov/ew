%%%-------------------------------------------------------------------
%%% File    : ew_server.erl
%%% Author  : selead <allselead@gmail.com>
%%% Description : 
%%%
%%% Created : 12 Jul 2010 by selead <allselead@gmail.com>
%%%-------------------------------------------------------------------
-module(ew_server).

-behaviour(gen_server).

-define(TCP_OPTIONS,  {reuseaddr, true}, {active, false}, {backlog,30}).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-compile(export_all). 
% -export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3, create/2]).

-record(state, {listen_socket, port, acceptor}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Port) when is_integer(Port) ->
    Name = list_to_atom(lists:flatten(io_lib:format("ew_~p", [Port]))),
    io:fwrite("GS start link~n", [ ]),
    gen_server:start_link({local, Name}, ?MODULE, [Port], []).

%%====================================================================
%% Server functions
%%====================================================================

%% create new acceptor
create(ServerPid, Pid) ->
    io:fwrite("Before create!", [ ]),
    gen_server:cast(ServerPid, {create, Pid}).

stop(ServerPid) ->
    io:fwrite("stop here~n", [ ]),
    gen_server:cast(ServerPid, stop).

echo(ServerPid) ->
    io:fwrite("Echoo..... ~n", [ ]),
    gen_server:call(ServerPid, echo).

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Port]) ->
%%    process_flag(trap_exit, true),
    io:fwrite("Gen server init~n", [ ]),
    case gen_tcp:listen(Port, [binary, ?TCP_OPTIONS]) of
	{ok, LSocket} ->
	    %% create accepting process
	    Pid = ew_socket:start_link(self(), LSocket, Port),
	    io:fwrite("Create accepting process: (~p | ~p)~n", [LSocket, Port]),
	    {ok, #state{listen_socket = LSocket, port = Port, acceptor = Pid}};
	{error, Reason} ->
	    {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(echo, From,  _State)->
    io:fwrite("Echo...", [ ]),
    {reply, ok, _State};



handle_call(Request, From, State) ->
    io:fwrite("Handle call: ~p~n", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
%% Called by gen_server framework when the cast message from create/2 is received
handle_cast({create, _Pid}, #state{listen_socket = LSocket} = State) ->
    New_pid = ew_socket:start_link(self(), LSocket, State#state.port),
    io:fwrite("Create new socket with pid = ~p~n", [New_pid]),
    {noreply, State#state{acceptor=New_pid}};

handle_cast(stop, State) ->
    io:fwrite("terminate in hangle cast ...~n", [ ]),
    {stop, normal, State};

handle_cast(Msg, State) ->
    io:fwrite("Get message: ~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, normal}, #state{acceptor=Pid} = State) ->
    {noreply, State};

%% The current acceptor has died, wait a little and try again
handle_info({'EXIT', Pid, _Abnormal}, #state{acceptor=Pid} = State) ->
    timer:sleep(1000),
    ew_socket:start_link(self(), State#state.listen_socket, State#state.port),
    {noreply, State};

handle_info(Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    gen_tcp:close(State#state.listen_socket),
    ok.


%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
