%%in standard output
-define(PRINT(Format, Args), io:format(Format, Args)).
-define(PRINT(Format), io:format(Format)).

-ifdef(LIVE).
	-define(_U(S),unicode:characters_to_list(iolist_to_binary(S))).
-else.
	-define(_U(S),S).
-endif.

%% define logger
-define(DEBUG(Format, Args), logger:debug_msg(?MODULE,?LINE,Format, Args)).
-define(INFO(Format, Args), logger:info_msg(?MODULE,?LINE,Format, Args)).
-define(WARNING(Format, Args), logger:warning_msg(?MODULE,?LINE,Format, Args)).
-define(WARNING2(Format, Args), logger:warning_msg(?MODULE,?LINE,Format ++ ",~w", Args ++ [erlang:get_stacktrace()])).
-define(ERROR(Format, Args), logger:error_msg(?MODULE,?LINE,Format, Args)).		
-define(CRITICAL_MSG(Format, Args), logger:critical_msg(?MODULE,?LINE,Format, Args)).

%% no param logger
-define(DEBUG(Format), logger:debug_msg(?MODULE,?LINE,Format, [])).
-define(INFO(Format), logger:info_msg(?MODULE,?LINE,Format, [])).
-define(WARNING(Format), logger:warning_msg(?MODULE,?LINE,Format, [])).
-define(WARNING2(Format), logger:warning_msg(?MODULE,?LINE,Format ++ ",~w", [] ++ [erlang:get_stacktrace()])).
-define(ERROR(Format), logger:error_msg(?MODULE,?LINE,Format, [])).		
-define(CRITICAL_MSG(Format), logger:critical_msg(?MODULE,?LINE,Format, [])).
