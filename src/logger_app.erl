-module(logger_app).

-behaviour(application).

%% Application callbacks
-export([start/0,start/2, stop/1]).

%% Start Function
start() ->
    application:start(logger).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start(Type, StartArgs) -> {ok, Pid} |
%%                                 {ok, Pid, State} |
%%                                 {error, Reason}
%% @doc This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end 
%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
	LogLevel = case application:get_env(logger, log_level) of
		{ok, Level} -> Level;
		_ -> 6
	end,
	loglevel:set(LogLevel),
	LogPath = case application:get_env(logger, path_log) of
		{ok, L} -> L;
		_ -> "./logs"
	end,
	error_logger:add_report_handler(logger_h, LogPath),
	{ok, self()}.

%%--------------------------------------------------------------------
%% @spec stop(State) -> void()
%% @doc This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%% @end 
%%--------------------------------------------------------------------
stop(_State) ->
	error_logger:delete_report_handler(logger_h),
	ok.

%%====================================================================
%% Internal functions
%%====================================================================

