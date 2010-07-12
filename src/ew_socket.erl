%%%-------------------------------------------------------------------
%%% File    : ew_socket.erl
%%% Author  : selead <allselead@gmail.com>
%%% Description :
%%%
%%% Created : 12 Jul 2010 by selead <allselead@gmail.com>
%%%-------------------------------------------------------------------
-module(ew_socket).

-compile(export_all).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
% -export([start_link/3
%         ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
% -export([init/1
%         ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
%% maximum idle timeout
-define(MAX_IDLE_TIMEOUT, 1000*10). %% 10 seconds

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(connection, {socket, port, peer_addr, peer_port}).

-record(request, {connection=keep_alive,	        % keep_alive | close
	      content_length,                   % Integer
	      vsn,                              % {Maj,Min}
	      method,                           % 'GET'|'POST'
	      uri,				% Truncated URI /index.html
              args="",                          % Part of URI after ?
	      headers,				% [{Tag, Val}]
	      body = <<>>}).			% Content Body


%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function:
%% Description:
%%--------------------------------------------------------------------
start_link(LPid, LSocket, LPort) ->
    proc_lib:spawn_link(?MODULE, init, [{LPid, LSocket, LPort}]).

%% get from iserve_socket
init({LPid, LSocket, LPort}) ->
    case catch gen_tcp:accept(LSocket) of
	{ok, Socket} ->

	    % io:fwrite("Address ~p Port ~p~n", [Addr, Port]),
	    io:fwrite("Module = ~p~n", [?MODULE]),
	    % io:fwrite("Accept socket HERE ~p~n", [Socket]),
	    % io:fwrite("Lpid = ~p, Self pid = ~p~n", [LPid, self()]),
	    ew_server:create(LPid, self()),
	    io:fwrite("After FORK ~p~n", [Socket]),
            % C = #c{sock = Socket,
            %        port = ListenPort,
            %        peer_addr = Addr,
            %        peer_port = Port},
	    {ok, {Addr, Port}} = inet:peername(Socket),
	    Con = #connection{socket=Socket, port=LPort, peer_addr=Addr, peer_port=Port},
	    get_request(Socket, Con, [], LPort); %% Jump to state 'request'
	Error ->
	    io:fwrite("Exit on error ~p~n", [Error]),
	    exit({error, accept_failed})
    end.


%%====================================================================
%% Internal functions
%%====================================================================



% server() ->
%     {ok, ListenSocket} = gen_tcp:listen(1234, [binary, {active, false}]),
%     wait_connect(ListenSocket,0).

% wait_connect(ListenSocket, Count) ->
%     {ok, Socket} = gen_tcp:accept(ListenSocket), spawn(?MODULE, wait_connect, [ListenSocket, Count+1]), get_request(Socket, [], Count).

get_request(Socket, Connection, BinaryList, Port) ->
    io:fwrite("Get request~n", [ ]),
    case gen_tcp:recv(Socket, 0, 10000) of
	{ok, Binary} -> get_request(Socket, Connection, [Binary|BinaryList], Port);
	{error, closed} ->
	    handle(lists:reverse(BinaryList), Connection, Port);
	{error, timeout} ->
	    io:fwrite("Close request by timeout~n", [ ]),
	    handle(lists:reverse(BinaryList), Connection, Port)
    end.


handle(Binary, Connection, Port) ->
    {ok, Fd} = file:open("log_file_"++integer_to_list(Port), [append]),
    file:write(Fd, io_lib:format("Connection:  ~p:~p~n",
				 [Connection#connection.peer_addr,
				  Connection#connection.peer_port])),
    file:write(Fd, Binary),
    file:close(Fd).
