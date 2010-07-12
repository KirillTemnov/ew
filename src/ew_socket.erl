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

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(connection, {socket, port}).

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
	    % {ok, {Addr, Port}} = inet:peername(Socket),
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
	    Con = #connection{socket=Socket, port=LPort},
	    get_request(Socket, [], 2); %% Jump to state 'request'
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

get_request(Socket, BinaryList, Count) ->
    io:fwrite("Get request~n", [ ]),
    case gen_tcp:recv(Socket, 0, 10000) of
	{ok, Binary} -> get_request(Socket, [Binary|BinaryList], Count);
	{error, closed} ->
	    handle(lists:reverse(BinaryList), Count);
	{error, timeout} ->
	    io:fwrite("Close request by timeout~n", [ ]),
	    handle(lists:reverse(BinaryList), Count)
    end.


handle(Binary, Count) ->
    {ok, Fd} = file:open("log_file_"++integer_to_list(Count), write),
    file:write(Fd, Binary),
    file:close(Fd).
