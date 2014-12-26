-module(test).
-include("logger.hrl").

-export([test/0,
         info/1,
         info/2,
         debug/1,
         debug/2,
         warning/1,
         warning/2
        ]).

test()->
	?INFO("Test info:~w",[1]),
	?DEBUG("Test debug:~w",[2]),
	?WARNING("Test warning:~w",[3]),
	?WARNING2("Test warning2:~w",[4]),
	?ERROR("Test error:~w",[5]),
	ok.

info(Msg) ->
    info(Msg,[]).
info(Msg,Info) ->
    ?INFO(Msg,Info).

debug(Msg) ->
    debug(Msg,[]).
debug(Msg,Info) ->
    ?DEBUG(Msg,Info).

warning(Msg) ->
    warning(Msg,[]).
warning(Msg,Info) ->
    ?WARNING(Msg,Info).

error(Msg) ->
    error(Msg,[]).
error(Msg,Info) ->
    ?ERROR(Msg,Info).
