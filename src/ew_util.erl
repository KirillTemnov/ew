%%%-------------------------------------------------------------------
%%% File    : ew_util.erl
%%% Author  : selead <allselead@gmail.com>
%%% Description : 
%%%
%%% Created : 12 Jul 2010 by selead <allselead@gmail.com>
%%%-------------------------------------------------------------------
-module(ew_util).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: split_string/2
%% Description: Split string in two substrings by Char
%% Example:
%%  split_string("string1-string2", $-).
%%  {"string1", "string2"}
%%--------------------------------------------------------------------
split_string(String, Char) ->
    split_string(String, [], Char).

%%--------------------------------------------------------------------
%% Function: get_host_and_port/1
%% Description: Split host string into host (String) and port (Integer).
%% Example:
%%  get_host_and_port("localhost").
%%  {"localhost", 80}
%%  get_host_and_port("localhost:8080").
%%  {"localhost", 8080}
%%--------------------------------------------------------------------
get_host_and_port(HostString) ->
    {Host, Port0} = split_string(HostString, $:),
    Port = case Port0 of
	       [] -> 80;
	       _ -> list_to_integer(Port0)
	   end,
    {Host, Port}.

%%====================================================================
%% Internal functions
%%====================================================================

split_string([], Acc, Char) ->
    {lists:reverse(Acc), []};
split_string([Char|String], Acc, Char) ->
    {lists:reverse(Acc), String};
split_string([Chr|String], Acc, Char) ->
    split_string(String, [Chr|Acc], Char).
