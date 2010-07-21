%%%-------------------------------------------------------------------
%%% File    : util.hrl
%%% Author  : selead <allselead@gmail.com>
%%% Description : Utils macros for ew server
%%%
%%% Created : 21 Jul 2010 by selead <allselead@gmail.com>
%%%-------------------------------------------------------------------

%% macros for logging messages
-define(LOG_ERROR(Message, VarsList),
	io:fwrite(lists:concat(["ERROR   : [",?MODULE,": ", ?LINE, "] ", Message]), VarsList)).

-ifdef(debug).
-define(LOG_DEBUG(Message, VarsList),
	io:fwrite(lists:concat(["DEBUG   : [",?MODULE,": ", ?LINE, "] ", Message]), VarsList)).
-define(LOG_WARNING(Message, VarsList),
	io:fwrite(lists:concat(["WARNING : [",?MODULE,": ", ?LINE, "] ", Message]), VarsList)).
-else.
-define(LOG_DEBUG(Message, VarsList), ok).
-define(LOG_WARNING(Message, VarsList), ok).
-endif.

-define(LOG_INFO(Message, VarsList),
	io:fwrite(lists:concat(["INFO    : [",?MODULE,": ", ?LINE, "] ", Message]), VarsList)).


