%%%-------------------------------------------------------------------
%%% File    : ew_mgr.erl
%%% Author  : selead <allselead@gmail.com>
%%% Description : This module is a manager for handle proxy connections.
%%%
%%% Created : 13 Jul 2010 by selead <allselead@gmail.com>
%%%-------------------------------------------------------------------
-module(ew_mgr).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include ("ew_mgr.hrl").
%%--------------------------------------------------------------------
%% External exports
-compile(export_all).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {routing}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: stop/0
%% Description: Shutdown server.
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast({global, ?MODULE}, stop).

%%--------------------------------------------------------------------
%% Function: add_route/1
%% Description: Add new route to manager
%%
%% Example:
%%  ew_mgr:add_route(#web_route{host="localhost", port=80, proxy_host="google.com", proxy_port=80}).
%%--------------------------------------------------------------------
add_route(Route) ->
    gen_server:call({global, ?MODULE}, {add_route, Route}).

%% remove route
%%--------------------------------------------------------------------
%% Function: remove_route/1
%% Description: Remove route from manager.
%%
%% Example:
%%  ew_mgr:remove_route(#web_route{host="localhost", port=80, proxy_host="google.com", proxy_port=80}).
%%--------------------------------------------------------------------
remove_route(Route) ->
    gen_server:call({global, ?MODULE}, {remove_route, Route}).


%%--------------------------------------------------------------------
%% Function: list_routes/0
%% Description: Get list of all routes.
%%--------------------------------------------------------------------
list_routes() ->
    gen_server:call({global, ?MODULE}, list_routes).

%%--------------------------------------------------------------------
%% Function: list_routes/1 , list_routes/2
%% Description: Filter list of routes by host and port (default 80)
%%--------------------------------------------------------------------
list_routes(Host) ->
    list_routes(Host, 80).

list_routes(Host, Port) when is_integer(Port) ->
    gen_server:call({global, ?MODULE}, {list_routes, Host, Port}).

%%--------------------------------------------------------------------
%% Function: request_to_route/1
%% Description: Function should be called on each new request passing
%%              through the route.
%% Returns: ok | error
%%--------------------------------------------------------------------
request_to_route(#web_route{first_request_date=FirstDate}=Route) when FirstDate == [] ->
    request_to_route(Route#web_route{first_request_date=calendar:now_to_datetime(erlang:now())});

request_to_route(#web_route{requests=Requests, requests_total=RequestsTotal}=Route) ->
    {_, NowSeconds, _} = erlang:now(),
    case Route#web_route.now_seconds of
	NowSeconds ->				% same second request
	    case Route#web_route.requests > Route#web_route.rps_max of
		true ->
		    io:fwrite("ROUTE OVERFLOW (~p)~n, must create more routes!~n", [Route]),
		    error;
		_ ->				% make a normal request
		    gen_server:call({global, ?MODULE}, {increment_route_request, Route}),
		    ok
	    end;
	_ ->
	    gen_server:call({global, ?MODULE}, {update_route, Route, NowSeconds}),
	    ok
    end.


%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, dict:new()}.

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
handle_call({add_route, Route}, From, State) ->
    NewState = dict:store(get_route_name(Route), Route, State),
    {reply, ok, NewState};

handle_call({increment_route_request, #web_route{requests=Requests, requests_total=RequestsTotal}=Route}, From, State) ->
    NewState = dict:store(get_route_name(Route), Route#web_route{requests=Requests+1, requests_total=RequestsTotal+1}, State),
%%    io:fwrite("Inc request [~p]~n", [Requests+1]),
    {reply, ok, NewState};

handle_call({update_route,  #web_route{requests_total=RequestsTotal}=Route, NewSeconds}, From, State) ->
    NewState = dict:store(get_route_name(Route),
			  Route#web_route{now_seconds=NewSeconds, requests_total=RequestsTotal+1, requests=0},
			  State),
%%    io:fwrite("Update request [~p]~n", [R]),
    {reply, ok, NewState};

handle_call(list_routes, From, State) ->
    {reply, dict:to_list(State), State};

handle_call({remove_route, Route}, From, State) ->
    {reply, ok, dict:erase(get_route_name(Route), State)};

handle_call({list_routes, Host, Port}, From, State) ->
    FilteredDict =  dict:filter(
		      fun(Key, #web_route{host=THost, port=TPort} = Value) ->
			      Host =:= THost andalso Port =:= TPort end, State),
    {reply, dict:to_list(FilteredDict), State};

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


handle_cast(stop, State) ->
    io:fwrite("Terminate ~p ..... ok~n", [?MODULE]),
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
handle_info({'EXIT', Pid, normal}, State) ->
    {noreply, State};

handle_info({'EXIT', Pid, Abnormal}, State) ->
    io:fwrite("catch 'EXIT' with message: ~p~n", [Abnormal]),
    {noreply, State};

handle_info(Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
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

%%--------------------------------------------------------------------
%% Function: get_route_name/1
%% Description: Get string name of route by concatenating host, port
%%              proxy_host and proxy_port.
%%--------------------------------------------------------------------
get_route_name(#web_route{host=Host, port=Port, proxy_host=PHost, proxy_port=PPort} = Route) ->
    lists:flatten(io_lib:format("~p : ~p -> ~p : ~p ", [Host, Port, PHost, PPort])).


%%--------------------------------------------------------------------
%% Function: update_route/1
%% Description: Update an existing route (change stat info)
%%--------------------------------------------------------------------
update_route(Route) ->
    gen_server:call({global, ?MODULE}, {update_route, Route}).


