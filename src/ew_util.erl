%%%-------------------------------------------------------------------
%%% File    : ew_util.erl
%%% Author  : selead <allselead@gmail.com>
%%% Description : Helpers module for others ew modules.
%%%
%%% Created : 12 Jul 2010 by selead <allselead@gmail.com>
%%%-------------------------------------------------------------------
-module(ew_util).
-vsn("0.1.5").

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
% -export([
%         ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
% -export([
%         ]).

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

%%--------------------------------------------------------------------
%% Function: get_host_string/2
%% Description: if port is 80 return Host, othewise return "Host:Port"
%%--------------------------------------------------------------------
get_host_string(Host, Port)  when is_integer(Port) ->
    case Port of
	 80 -> Host;
	 _ -> Host ++ ":" ++ Port
    end.

%%--------------------------------------------------------------------
%% Function: dump_to_file/2
%% Description: Dump data to file. This function for debug purposes.
%%--------------------------------------------------------------------
dump_to_file(Filename, Data) ->
    {ok, Fd} = file:open(Filename, [write]),
    file:write(Fd, Data),
    file:close(Fd).


%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: split_string/3
%% Description: Split string, aux function to split_string/2.
%%--------------------------------------------------------------------
split_string([], Acc, Char) ->
    {lists:reverse(Acc), []};
split_string([Char|String], Acc, Char) ->
    {lists:reverse(Acc), String};
split_string([Chr|String], Acc, Char) ->
    split_string(String, [Chr|Acc], Char).
