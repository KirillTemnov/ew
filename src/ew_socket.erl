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
-include ("ew_mgr.hrl").
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
%% maximum timeout before recieve post data
-define(MAX_POST_TIMEOUT, 1000*30). %% 30 seconds

-define(NOT_IMPLEMENTED, "HTTP/1.1 501 Not Implemented\r\n\r\n").
-define(FORBIDDEN, "HTTP/1.1 403 Forbidden\r\n\r\n").
-define(NOT_FOUND, "HTTP/1.1 404 Not Found\r\n\r\n").


%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(connection, {socket, port, peer_addr, peer_port}).

-record(request, {connection=keep_alive,	        % keep_alive | close
		  content_length,                       % Integer
		  host,                                 % Hostname, string
		  vsn,                                  % {Maj,Min}
		  method,                               % 'GET'|'POST'
		  uri,	        			% Truncated URI /index.html
		  args="",                              % Part of URI after ?
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
	    ew_server:create(LPid, self()),
	    io:fwrite("After FORK ~p~n", [Socket]),
	    {ok, {Addr, Port}} = inet:peername(Socket),
	    Con = #connection{socket=Socket, port=LPort, peer_addr=Addr, peer_port=Port},
	    io:fwrite("Get new request~n", [ ]),
	    get_request(Con, #request{}); %% Jump to state 'request'
	Error ->
	    io:fwrite("Exit on error XX ~p~n", [Error]),
%%	    exit({error, accept_failed})
	    {error, accept_failed}
    end.


%%====================================================================
%% Internal functions
%%====================================================================

get_request(Connection, Request) ->
    case gen_tcp:recv(Connection#connection.socket, 0, ?MAX_IDLE_TIMEOUT) of
%%	{ok, Binary} -> get_request(Connection, [Binary|BinaryList]);

	{ok, {http_request, Method, Path, Version}} ->
	    get_headers(Connection, Request#request{vsn = Version,
						method = Method,
						uri = Path}, []);

	{error, {http_error, "\r\r"}} ->
	    get_request(Connection, Request);
	{error, {http_error, "\n"}} ->
	    get_request(Connection, Request);
	{error, closed} ->
	    close;
	{error, timeout} ->
	    io:fwrite("Close request by timeout~n", [ ]),
	    close
    end.

get_headers(Connection, Request, HdrList) ->
    case gen_tcp:recv(Connection#connection.socket, 0, ?MAX_IDLE_TIMEOUT) of

	{ok, {http_header, _, 'Host', _, Host}} ->
	    get_headers(Connection, Request#request{host = Host}, [{'Host', Host} | HdrList]);

	{ok, {http_header, _, 'Content-Length', _, CLen}} ->
	    Len = list_to_integer(CLen),
	    get_headers(Connection, Request#request{content_length = Len},
			[{'Content-Length', Len} | HdrList]);

	{ok, {http_header, _, 'Connection', _, Conn}} ->
	    KeepAlive = keep_alive(Request#request.vsn, Conn),
	    get_headers(Connection, Request#request{connection = KeepAlive},
			[{'Connection', Conn} | HdrList]);

	{ok, {http_header, _, Header, _, Val}} ->
	    get_headers(Connection, Request, [{Header, Val} | HdrList]);

	{error, {http_error, "\r\n" }} ->
	    get_headers(Connection, Request, HdrList);

	{error, {http_error, "\r" }} ->
	    get_headers(Connection, Request, HdrList);

	{ok, http_eoh} ->
	    get_body(Connection, Request#request{headers = lists:reverse(HdrList)});

	_Other ->
	    exit(normal)
    end.

%% Return keep alive status, that can be override by header value,
%% default for HTTP/1.1 - keep_alive, default for HTTP/1.0 - close.
keep_alive({1,1}, "close")      -> close;
keep_alive({1,1}, "Close")      -> close;
keep_alive({1,1}, _)            -> keep_alive;
keep_alive({1,0}, "Keep-Alive") -> keep_alive;
keep_alive(_, _)                -> close.

%% get request body

proceed_get_body_non_post_request(#connection{socket=Socket} = Connection, Request) ->
    case handle_get(Connection, Request) of
	close ->
	    gen_tcp:close(Socket);
	keep_alive ->
	    inet:setopts(Socket, [{packet, http}]),
	    get_request(Connection, #request{})
    end.

get_body(#connection{socket=Socket} = Connection, Request) ->
    case Request#request.method of
	'GET' ->
	    proceed_get_body_non_post_request(Connection, Request);
	'HEAD' ->
	    proceed_get_body_non_post_request(Connection, Request);
	'PUT' ->
	    proceed_get_body_non_post_request(Connection, Request);
	'DELETE' ->
	    proceed_get_body_non_post_request(Connection, Request);
	'POST' when is_integer(Request#request.content_length) ->
	    inet:setopts(Socket, [{packet, raw}]),
	    case gen_tcp:recv(Socket, Request#request.content_length, ?MAX_POST_TIMEOUT) of
		{ok, Bin} ->
		    case handle_post(Connection, Request#request{body = Bin}) of
			close ->
			    gen_tcp:close(Socket);
			keep_alive ->
			    inet:setopts(Socket, [{packet, http}]),
			    get_request(Connection, #request{})
		    end;
		_Other ->
		    exit(normal)
	    end;
	_Other -> %% todo implement other methods
	    send(Connection, ?NOT_IMPLEMENTED),
	    exit(normal)
    end.

handle_get(Connection, #request{host=Host} = Request) ->
    case Request#request.uri of
	{abs_path, FullPath} ->
	    {Path, Args} = ew_util:split_string(FullPath, $?),
	    call_mfa(Host,  Path, Args, Connection, Request);
%	    ConnState;
	{absoluteURI, http, _Host, _, FullPath} ->
	    {Path, Args} = ew_util:split_string(FullPath, $?),
	    call_mfa(Host,  Path, Args, Connection, Request);
%	    ConnState;
	{absoluteURI, _Other_method, _Host, _, FullPath} ->
	    io:fwrite("Other method: ~p~n", [_Other_method]),
	    %% todo implement other methods
	    send(Connection, ?NOT_IMPLEMENTED),
	    close;
	{absoluteURI, _Scheme, _RequestString} ->
	    send(Connection, ?NOT_IMPLEMENTED),
	    close;
	_ ->
	    send(Connection, ?FORBIDDEN),
	    close
    end.


%% implement post
handle_post(Connection, #request{connection=ConnState, host=Host} = Request) ->
    send(Connection, ?FORBIDDEN),
    close.

call_mfa(Host0, Path, Args, #connection{socket=Socket}=Connection, Request) ->
    {Host, Port} = ew_util:get_host_and_port(Host0),
%%    io:fwrite("Host = ~p   Port = ~p ~n", [Host, Port]),
    ListRoutes = ew_mgr:list_routes(Host, Port),
%%    io:fwrite("Get list of routes: ~p~n", [ListRoutes]),
    case ListRoutes of				% add case for 1+ web routes
	[] ->
%%	    io:fwrite("Close connection : not found", [ ]),
	    ok = send_to_socket(Socket, ?NOT_FOUND),
	    ok = gen_tcp:close(Socket),
	    close;
	[{_, WebRoute}] ->
%%	    io:fwrite("update-route (Socket = ~p)~n", [Socket]),
	    get_proxy_page(WebRoute, Path, Args, Connection, Request);
	_ ->
	    io:fwrite("Error in call mfa!", [ ])
    end.
%     Body =  list_to_binary(io_lib:format("<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
% <html>
% <head>
%   <title>ew test page</title>
% </head>
% <body>
%   <h2>Host: ~p </h2>
%   <h2>Path: ~p </h2>
%   <h2>Args: ~p </h2>
% </body>
% </html>", [Host0, Path, Args])),
%     Headers = add_content_length([], Body),
%     Enc_headers = enc_headers(Headers),
%     Resp = [<<"HTTP/1.1 200 OK\r\n">>,
% 	    Enc_headers,
% 	    <<"\r\n">>,
% 	    Body],
%     send(Connection, Resp).

get_proxy_page(#web_route{proxy_host=PHost, proxy_port=PPort} = WebRoute, Path, Args, #connection{socket=ClientSocket}, Request) ->
    ok = ew_mgr:request_to_route(WebRoute),
    case gen_tcp:connect(PHost, PPort, [binary, {packet, raw}]) of
	{ok, Socket} ->
	    {Maj, Min} = Request#request.vsn,
	    Uri = Path ++ case Args of
			      [] ->
				  [];
			      _ ->
				  [$? | Args]
			  end,
	    First_line = list_to_binary(io_lib:format("~s ~s HTTP/~p.~p\r\n",
						      [Request#request.method,
						       Uri, Maj, Min])),
	    Data = [First_line, enc_headers(Request#request.headers), <<"\r\n">>,
		    Request#request.body],
	    inet:setopts(Socket, [{packet, http}, {active, false}]),
	    send_to_socket(Socket, Data),
	    case get_proxy_request(Socket, Data) of
		{error, timeout} ->
		    io:fwrite("Close proxy request by timeout~n", [ ]),
		    gen_tcp:close(Socket),
		    gen_tcp:close(ClientSocket),
		    {error, timeout};
		ReturnedData ->
		    inet:setopts(ClientSocket, [{packet, raw}, {active, false}]),
		    send_to_socket(ClientSocket, ReturnedData),
		    ok
	    end;
	{error, Error} ->
	    {error, Error}
    end.

get_proxy_request(Socket, BinaryList) ->
    inet:setopts(Socket, [{packet, raw}]),
    case gen_tcp:recv(Socket, 0, 5000) of
	{ok, Binary} ->
	    get_proxy_request(Socket, [Binary|BinaryList]);
	{error, closed} ->
	    lists:reverse(BinaryList);
	{error, timeout} ->
	    {error, timeout}
    end.

enc_headers([]) ->
    [];
enc_headers([{Tag, Val}|T]) when is_atom(Tag) ->
    [atom_to_list(Tag), ": ", enc_header_val(Val), "\r\n"|enc_headers(T)];
enc_headers([{Tag, Val}|T]) when is_list(Tag) ->
    [Tag, ": ", enc_header_val(Val), "\r\n"|enc_headers(T)].



enc_header_val(Val) when is_atom(Val) ->
    atom_to_list(Val);
enc_header_val(Val) when is_integer(Val) ->
    integer_to_list(Val);
enc_header_val(Val) ->
    Val.


add_content_length(Headers, Body) ->
    case lists:keysearch('Content-Length', 1, Headers) of
        {value, _} ->
            Headers;
        false ->
            [{'Content-Length', size(Body)}|Headers]
    end.

%% send data to the socket
send(#connection{socket=Socket}, Data) ->
    send_to_socket(Socket, Data).

send_to_socket(Socket, Data) ->
    case gen_tcp:send(Socket, Data) of
	ok -> ok;
	_  -> exit(normal)
    end.

dump_to_file(Filename, Data) ->
    {ok, Fd} = file:open(Filename, [write]),
    file:write(Fd, Data),
    file:close(Fd).

