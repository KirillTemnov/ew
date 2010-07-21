%%%-------------------------------------------------------------------
%%% File    : ew_socket.erl
%%% Author  : selead <allselead@gmail.com>
%%% Description : Socket for accepting connections and passing them to proxy server.
%%%
%%% Created : 12 Jul 2010 by selead <allselead@gmail.com>
%%%-------------------------------------------------------------------
-module(ew_socket).

-compile(export_all).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include ("ew_mgr.hrl").
-include ("util.hrl").

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
%% max time to wait responce from proxy
-define(MAX_PROXY_TIMEOUT, 1000*5).

%% standart headers (move to ew_mgr ?)
-define(NOT_IMPLEMENTED, "HTTP/1.1 501 Not Implemented\r\n\r\n").
-define(FORBIDDEN, "HTTP/1.1 403 Forbidden\r\n\r\n").
-define(NOT_FOUND, "HTTP/1.1 404 Not Found\r\n\r\n").


%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(request, {connection=keep_alive,	        % keep_alive | close
		  content_length,                       % Integer
		  host,                                 % Hostname, string
		  vsn,                                  % {Maj,Min}
		  method,                               % 'GET'|'HEAD'|'PUT'|'POST'|'DELETE'
		  uri,	        			% uri without domain
		  args="",                              % Part of uri after ?
		  headers,				% [{Tag, Value}]
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

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initialize (open) new socket, and call of ew_server to
%%              accept new connections
%%--------------------------------------------------------------------
init({LPid, LSocket, LPort}) ->
    case catch gen_tcp:accept(LSocket) of
	{ok, Socket} ->
	    ew_server:create(LPid, self()),
	    ?LOG_INFO("After FORK ~p~n", [Socket]),
	    get_request(Socket, #request{}); %% Jump to state 'request'
	Error ->
	    {error, accept_failed}
    end.


%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: get_request/2
%% Description: Get http request, fill #request version, method and uri
%%--------------------------------------------------------------------
get_request(Socket, Request) ->
    case gen_tcp:recv(Socket, 0, ?MAX_IDLE_TIMEOUT) of

	{ok, {http_request, Method, Path, Version}} ->
	    get_headers(Socket, Request#request{vsn = Version,
						method = Method,
						uri = Path}, []);

	{error, {http_error, "\r\r"}} ->
	    get_request(Socket, Request);
	{error, {http_error, "\n"}} ->
	    get_request(Socket, Request);
	{error, closed} ->
	    close;
	{error, timeout} ->
	    ?LOG_WARNING("Close request by timeout socket = ~p~n", [Socket]),
	    close
    end.

%%--------------------------------------------------------------------
%% Function: get_headers/3
%% Description: Get http request headers and fill #request header,
%%                  host and content_length.
%%--------------------------------------------------------------------
get_headers(Socket, Request, HdrList) ->
    case gen_tcp:recv(Socket, 0, ?MAX_IDLE_TIMEOUT) of

	{ok, {http_header, _, 'Host', _, Host}} ->
	    get_headers(Socket, Request#request{host = Host}, [{'Host', Host} | HdrList]);

	{ok, {http_header, _, 'Content-Length', _, CLen}} ->
	    Len = list_to_integer(CLen),
	    get_headers(Socket, Request#request{content_length = Len},
			[{'Content-Length', Len} | HdrList]);

	{ok, {http_header, _, 'Connection', _, Conn}} ->
	    KeepAlive = keep_alive(Request#request.vsn, Conn),
	    get_headers(Socket, Request#request{connection = KeepAlive},
			[{'Connection', Conn} | HdrList]);

	{ok, {http_header, _, Header, _, Val}} ->
	    get_headers(Socket, Request, [{Header, Val} | HdrList]);

	{error, {http_error, "\r\n" }} ->
	    get_headers(Socket, Request, HdrList);

	{error, {http_error, "\r" }} ->
	    get_headers(Socket, Request, HdrList);

	{ok, http_eoh} ->			% headers finished, continue reading request
	    get_body(Socket, Request#request{headers = lists:reverse(HdrList)});

	_Other ->
	    exit(normal)
    end.


%%--------------------------------------------------------------------
%% Function: keep_alive/2
%% Description: Return keep alive status, that can be override by
%%              header value,  default for HTTP/1.1 - keep_alive,
%%              default for HTTP/1.0 - close
%%--------------------------------------------------------------------
keep_alive({1,1}, "close")      -> close;
keep_alive({1,1}, "Close")      -> close;
keep_alive({1,1}, _)            -> keep_alive;
keep_alive({1,0}, "Keep-Alive") -> keep_alive;
keep_alive(_, _)                -> close.

%%--------------------------------------------------------------------
%% Function: proceed_get_body_non_post_request/2
%% Description: Function-wrapper for any non-POST request.
%%--------------------------------------------------------------------
proceed_get_body_non_post_request(Socket, Request) ->
    case handle_request(Socket, Request) of
	close ->
	    gen_tcp:close(Socket);
	keep_alive ->
	    inet:setopts(Socket, [{packet, http}]),
	    get_request(Socket, #request{})
    end.

%%--------------------------------------------------------------------
%% Function: get_body/2
%% Description: Read request body (if presents) and pass request to
%%                   proxy host.
%%--------------------------------------------------------------------
get_body(Socket, Request) ->
    case Request#request.method of
	'GET' ->
	    proceed_get_body_non_post_request(Socket, Request);
	'HEAD' ->
	    proceed_get_body_non_post_request(Socket, Request);
	'PUT' ->
	    proceed_get_body_non_post_request(Socket, Request);
	'DELETE' ->
	    proceed_get_body_non_post_request(Socket, Request);
	'POST' when is_integer(Request#request.content_length) ->
	    inet:setopts(Socket, [{packet, raw}]),
	    case gen_tcp:recv(Socket, Request#request.content_length, ?MAX_POST_TIMEOUT) of
		{ok, Bin} ->
		    case handle_request(Socket, Request#request{body = Bin}) of
			close ->
			    gen_tcp:close(Socket);
			keep_alive ->
			    inet:setopts(Socket, [{packet, http}]),
			    get_request(Socket, #request{})
		    end;
		_Other ->
		    exit(normal)
	    end;
	_Other -> %% todo maybe implement other methods?
	    send(Socket, ?NOT_IMPLEMENTED),
	    exit(normal)
    end.

%%--------------------------------------------------------------------
%% Function: handle_request/2
%% Description: Handle request and pass it to proxy by
%%              calling handle_proxy_request/5
%%--------------------------------------------------------------------
handle_request(Socket, #request{host=Host, connection=ConnState} = Request) ->
    case Request#request.uri of
	{abs_path, FullPath} ->
	    {Path, Args} = ew_util:split_string(FullPath, $?),
	    handle_proxy_request(Host,  Path, Args, Socket, Request),
	    ConnState;
	{absoluteURI, http, _Host, _, FullPath} ->
	    {Path, Args} = ew_util:split_string(FullPath, $?),
	    handle_proxy_request(Host,  Path, Args, Socket, Request),
	    ConnState;
	{absoluteURI, _Other_method, _Host, _, FullPath} ->
	    ?LOG_WARNING("Other method \"~p\" inside handle_request.", [_Other_method]),
	    send(Socket, ?NOT_IMPLEMENTED),
	    close;
	{absoluteURI, _Scheme, _RequestString} ->
	    send(Socket, ?NOT_IMPLEMENTED),
	    close;
	_ ->
	    send(Socket, ?FORBIDDEN),
	    close
    end.

%%--------------------------------------------------------------------
%% Function: handle_proxy_request/5
%% Description: Passing request to one of waiting proxies, according
%%              to ew_mgr list of routes
%%--------------------------------------------------------------------
handle_proxy_request(Host0, Path, Args, Socket, Request) ->
    {Host, Port} = ew_util:get_host_and_port(Host0),
    ListRoutes = ew_mgr:list_routes(Host, Port),
    case ListRoutes of				% add case for 1+ web routes
	[] ->
	    ok = send(Socket, ?NOT_FOUND),
	    ok = gen_tcp:close(Socket),
	    close;
	[{_, WebRoute}] ->
	    get_proxy_page(WebRoute, Path, Args, Socket, Request);
	_ ->
	    ?LOG_ERROR("Can't call proxy page for ~p:~p~n", [Host, Port])
    end.

%%--------------------------------------------------------------------
%% Function: get_proxy_page/5
%% Description: Get page from proxy and write it to client.
%%--------------------------------------------------------------------
get_proxy_page(#web_route{proxy_host=PHost, proxy_port=PPort} = WebRoute, Path, Args, ClientSocket, Request) ->
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
	    send(Socket, Data),
	    case read_binary_data_from_socket(Socket, []) of
		{error, timeout} ->
		    ?LOG_WARNING("Close proxy request to host ~p by timeout", [PHost]),
		    gen_tcp:close(Socket),
		    gen_tcp:close(ClientSocket),
		    {error, timeout};
		ReturnedData ->
		    inet:setopts(ClientSocket, [{packet, raw}, {active, false}]),
		    send(ClientSocket, ReturnedData),
		    ok
	    end;
	{error, Error} ->
	    {error, Error}
    end.

%%--------------------------------------------------------------------
%% Function: read_binary_data_from_socket/2
%% Description: Read all binary data from (proxy) socket.
%%--------------------------------------------------------------------
read_binary_data_from_socket(Socket, BinaryList) ->
    inet:setopts(Socket, [{packet, raw}]),
    case gen_tcp:recv(Socket, 0, ?MAX_PROXY_TIMEOUT) of
	{ok, Binary} ->
	    read_binary_data_from_socket(Socket, [Binary|BinaryList]);
	{error, closed} ->
	    lists:reverse(BinaryList);
	{error, timeout} ->
	    {error, timeout}
    end.

%%--------------------------------------------------------------------
%% Function: enc_headers/1
%% Description: Encode list of headers to list of lists (strings).
%%--------------------------------------------------------------------
enc_headers([]) ->
    [];
enc_headers([{Tag, Val}|T]) when is_atom(Tag) ->
    [atom_to_list(Tag), ": ", enc_header_val(Val), "\r\n"|enc_headers(T)];
enc_headers([{Tag, Val}|T]) when is_list(Tag) ->
    [Tag, ": ", enc_header_val(Val), "\r\n"|enc_headers(T)].


%%--------------------------------------------------------------------
%% Function: enc_header_val/1
%% Description: Encode header value to list (string)
%%--------------------------------------------------------------------
enc_header_val(Val) when is_atom(Val) ->
    atom_to_list(Val);
enc_header_val(Val) when is_integer(Val) ->
    integer_to_list(Val);
enc_header_val(Val) ->
    Val.

%%--------------------------------------------------------------------
%% Function: add_content_length/2
%% Description: Add content length to headers list.
%%--------------------------------------------------------------------
add_content_length(Headers, Body) ->
    case lists:keysearch('Content-Length', 1, Headers) of
        {value, _} -> Headers;
        false -> [{'Content-Length', size(Body)}|Headers]
    end.

%%--------------------------------------------------------------------
%% Function: send/2
%% Description: Send data to socket
%%--------------------------------------------------------------------
send(Socket, Data) ->
    case gen_tcp:send(Socket, Data) of
	ok -> ok;
	_  -> exit(normal)
    end.

