%%%----------------------------------------------------------------------
%%% File    : logger_h.erl
%%% Created : 2013-06-18
%%% Desc    : 参考自 ejabberd_logger 在原日志的基本上，加入缓存减少文件写入次数，提升日志吞吐量
%%%           设置新的日志文件 error_logger ! {emulator, _GL, reopen}
%%%----------------------------------------------------------------------

-module(logger_h).
-author('kqqsysu@gmail.com').

-behaviour(gen_event).

-include("logger.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
	 code_change/3, reopen_log/0, rotate_log/1]).

-record(state, {fd, file}).

-define(LOG_FILE_OPTION,[append, raw]).
-define(LOG_BASE_DIR,"./logs/").

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%%----------------------------------------------------------------------
init(BaseDir) ->
    filelib:ensure_dir(BaseDir ++ "/"),
    {Date, Time} = erlang:localtime(),
    File = lists:concat([BaseDir,"/",format_time(Date),"-",format_time(Time),".log"]),
    case file:open(File, ?LOG_FILE_OPTION) of
		{ok, Fd} ->
            set_log_ref(0),
	    	{ok, #state{fd = Fd, file = File}};
		Error ->
	    	Error
    end.

format_time(Time) ->
    Time2 =
    [ case V < 10 of 
            true -> 
                "0" ++ integer_to_list(V);
            false -> 
                integer_to_list(V) 
        end || V <- tuple_to_list(Time)],
    lists:concat(Time2).


%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_event(Event, State) ->
    write_event(State#state.fd, {erlang:localtime(), Event}),
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}                            
%%----------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_info(write_log,#state{fd = Fd} = State) ->
    do_write_log(Fd),
    set_log_ref(0),
    {ok,State};
handle_info({'EXIT', _Fd, _Reason}, _State) ->
    remove_handler;
handle_info({emulator, _GL, reopen}, State) ->
    file:close(State#state.fd),
    rotate_log(State#state.file),
    case file:open(State#state.file, ?LOG_FILE_OPTION) of
	{ok, Fd} ->
	    {ok, State#state{fd = Fd}};
	Error ->
	    Error
    end;
handle_info({emulator, GL, Chars}, State) ->
    write_event(State#state.fd, {erlang:localtime(), {emulator, GL, Chars}}),
    {ok, State};
handle_info(_Info, State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, #state{fd = Fd}) ->
    do_write_log(Fd),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

reopen_log() ->
    error_logger ! {emulator, noproc, reopen}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

% Copied from erlang_logger_file_h.erl
write_event(Fd, {Time, {error, _GL, {Pid, Format, Args}}}) ->
    T = write_time(Time),
    case catch io_lib:format(add_node(Format,Pid), Args) of
	S when is_list(S) ->
	    write_log(Fd, io_lib:format(T ++ S, []));
	    %% file:write(Fd, io_lib:format(T ++ S, []));
	_ ->
	    F = add_node("ERROR: ~p - ~p~n", Pid),
	    %% file:write(Fd, io_lib:format(T ++ F, [Format,Args]))
	    write_log(Fd, io_lib:format(T ++ F, [Format,Args]))
    end;
write_event(Fd, {Time, {emulator, _GL, Chars}}) ->
    T = write_time(Time),
    case catch io_lib:format(Chars, []) of
	S when is_list(S) ->
	    %% file:write(Fd, io_lib:format(T ++ S, []));
	    write_log(Fd, io_lib:format(T ++ S, []));
	_ ->
	    %% file:write(Fd, io_lib:format(T ++ "ERROR: ~p ~n", [Chars]))
	    write_log(Fd, io_lib:format(T ++ "ERROR: ~p ~n", [Chars]))
    end;
write_event(Fd, {Time, {info, _GL, {Pid, Info, _}}}) ->
    T = write_time(Time),
    %% file:write(Fd, io_lib:format(T ++ add_node("~p~n",Pid), [Info]));
    write_log(Fd, io_lib:format(T ++ add_node("~p~n",Pid), [Info]));
write_event(Fd, {Time, {error_report, _GL, {Pid, std_error, Rep}}}) ->
    T = write_time(Time),
    S = format_report(Rep),
    %% file:write(Fd, io_lib:format(T ++ S ++ add_node("", Pid), []));
    write_log(Fd, io_lib:format(T ++ S ++ add_node("", Pid), []));
write_event(Fd, {Time, {info_report, _GL, {Pid, std_info, Rep}}}) ->
    T = write_time(Time, "INFO REPORT"),
    S = format_report(Rep),
    %% file:write(Fd, io_lib:format(T ++ S ++ add_node("", Pid), []));
    write_log(Fd, io_lib:format(T ++ S ++ add_node("", Pid), []));
write_event(Fd, {Time, {info_msg, _GL, {Pid, Format, Args}}}) ->
    T = write_time(Time, "INFO REPORT"),
    case catch io_lib:format(add_node(Format,Pid), Args) of
	S when is_list(S) ->
	    %% file:write(Fd, io_lib:format(T ++ S, []));
	    write_log(Fd, io_lib:format(T ++ S, []));
	_ ->
	    F = add_node("ERROR: ~p - ~p~n", Pid),
	    %% file:write(Fd, io_lib:format(T ++ F, [Format,Args]))
	    write_log(Fd, io_lib:format(T ++ F, [Format,Args]))
    end;
write_event(_, _) ->
    ok.

format_report(Rep) when is_list(Rep) ->
    case string_p(Rep) of
	true ->
	    io_lib:format("~s~n",[Rep]);
	_ ->
	    format_rep(Rep)
    end;
format_report(Rep) ->
    io_lib:format("~p~n",[Rep]).

format_rep([{Tag,Data}|Rep]) ->
    io_lib:format("    ~p: ~p~n",[Tag,Data]) ++ format_rep(Rep);
format_rep([Other|Rep]) ->
    io_lib:format("    ~p~n",[Other]) ++ format_rep(Rep);
format_rep(_) ->
    [].

add_node(X, Pid) when is_atom(X) ->
    add_node(atom_to_list(X), Pid);
add_node(X, Pid) when node(Pid) /= node() ->
    lists:concat([X,"** at node ",node(Pid)," **~n"]);
add_node(X, _) ->
    X.

string_p([]) ->
    false;
string_p(Term) ->
    string_p1(Term).

string_p1([H|T]) when is_integer(H), H >= $\s, H < 255 ->
    string_p1(T);
string_p1([$\n|T]) -> string_p1(T);
string_p1([$\r|T]) -> string_p1(T);
string_p1([$\t|T]) -> string_p1(T);
string_p1([$\v|T]) -> string_p1(T);
string_p1([$\b|T]) -> string_p1(T);
string_p1([$\f|T]) -> string_p1(T);
string_p1([$\e|T]) -> string_p1(T);
string_p1([H|T]) when is_list(H) ->
    case string_p1(H) of
	true -> string_p1(T);
	_    -> false
    end;
string_p1([]) -> true;
string_p1(_) ->  false.

write_time(Time) -> write_time(Time, "ERROR REPORT").

write_time({{Y,Mo,D},{H,Mi,S}}, Type) ->
    io_lib:format("~n=~s==== ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w ===~n",
		  [Type, Y, Mo, D, H, Mi, S]).

%% @doc Rename the log file if exists, to "*-old.log".
%% This is needed in systems when the file must be closed before rotation (Windows).
%% On most Unix-like system, the file can be renamed from the command line and
%% the log can directly be reopened.
%% @spec (Filename::string()) -> ok
rotate_log(Filename) ->
    case file:read_file_info(Filename) of
	{ok, _FileInfo} ->
	    RotationName = filename:rootname(Filename),
	    file:rename(Filename, [RotationName, "-old-",get_old_index(),".log"]),
	    ok;
	{error, _Reason} ->
	    ok
    end.
-define(LOG_FILE_INDEX,log_file_index).
get_old_index() ->
	Index =
	case get(?LOG_FILE_INDEX) of
		N when is_integer(N) ->
			put(?LOG_FILE_INDEX,N+1),
			N;
		_ ->
			put(?LOG_FILE_INDEX,1),
			0
	end,
	lists:concat([Index]).
	
-define(LOG_LIST_CACHE,log_list_cache).
-define(LOG_CACHE_NUM,100).
-define(LOG_CACHE_REF,log_cache_ref).
get_log_cache() ->
    case get(?LOG_LIST_CACHE) of
        undefined ->
            [];
        L ->
            L
    end.
set_log_cache(L) ->
    put(?LOG_LIST_CACHE,L).

get_log_ref() ->
    get(?LOG_CACHE_REF).
set_log_ref(Ref) ->
    put(?LOG_CACHE_REF,Ref).

write_log(Fd,Msg) ->
    L = get_log_cache(),
    NewList = [Msg | L],
    case length(NewList) > ?LOG_CACHE_NUM of
        true ->
            do_write_log(Fd,NewList);
        false ->
            set_log_cache(NewList),
            case get_log_ref() of
                0 ->
                    Ref = erlang:send_after(500,self(),write_log),
                    set_log_ref(Ref);
                _ ->
                    skip
            end

    end,
    ok.

do_write_log(Fd) ->
    L = get_log_cache(),
    do_write_log(Fd,L).
do_write_log(Fd,L) ->
    file:write(Fd,lists:reverse(L)),
    set_log_cache([]),
    ok.


