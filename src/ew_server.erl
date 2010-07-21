%%%-------------------------------------------------------------------
%%% File    : ew_server.erl
%%% Author  : selead <allselead@gmail.com>
%%% Description : This module is a listening server for accept
%%%               incoming connections by http protocol.
%%%
%%% Created : 12 Jul 2010 by selead <allselead@gmail.com>
%%%-------------------------------------------------------------------
-module(ew_server).

-behaviour(gen_server).

-include ("util.hrl").

%% default TCP options
-define(TCP_OPTIONS,  {packet, http}, {reuseaddr, true}, {active, false}, {backlog,50}).


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
    ?LOG_INFO("GS start link ~n", []),
    gen_server:start_link({local, Name}, ?MODULE, [Port], []).

%%====================================================================
%% Server functions
%%====================================================================

%% create new acceptor
%%--------------------------------------------------------------------
%% Function: create/2
%% Description: Create new socket for accepting connection.
%%--------------------------------------------------------------------
create(ServerPid, Pid) ->
    gen_server:cast(ServerPid, {create, Pid}).

%%--------------------------------------------------------------------
%% Function: stop/1
%% Description: Stop server, listening incomming socket connection.
%%--------------------------------------------------------------------
stop(ServerPid) ->
    ?LOG_INFO("Stop server with pid = ~p~n", [ServerPid]),
    gen_server:cast(ServerPid, stop).

%%--------------------------------------------------------------------
%% Function: echo/1
%% Description: Debug echo function.
%%--------------------------------------------------------------------
echo(ServerPid) ->
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
    process_flag(trap_exit, true),		% this string for supervisor
    ?LOG_INFO("Init new http server on port ~p~n", [Port]),
    case gen_tcp:listen(Port, [binary, ?TCP_OPTIONS]) of
	{ok, LSocket} ->
	    %% create accepting process
	    case ew_socket:start_link(self(), LSocket, Port) of
		{error, accept_failed} ->
		    ?LOG_ERROR("Error opening accept socket on port ~p~n", [Port]),
		    {stop, socket_error};
		Pid ->
		    ?LOG_INFO("Create acceptiong process. Socket = ~p, port = ~p~n",
			      [LSocket, Port]),
		    {ok, #state{listen_socket = LSocket, port = Port, acceptor = Pid}}
	    end;
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
    ?LOG_INFO("Echo function call.", [ ]),
    {reply, ok, _State};


handle_call(Request, From, State) ->
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
    NewPid = ew_socket:start_link(self(), LSocket, State#state.port),
    ?LOG_INFO("Create new socket with pid = ~p~n", [NewPid]),
    {noreply, State#state{acceptor=NewPid}};

handle_cast(stop, State) ->
    ?LOG_INFO("Terminate server ...~n", [ ]),
    {stop, normal, State};

handle_cast(Msg, State) ->
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
