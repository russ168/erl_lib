-module(oma).
-compile(export_all).

%% +type unixtime() -> unix_time().
%%%% UNIX time, seconds since 1/1/1970 00:00:00 UTC.

unixtime() -> 
    {MS, S, _} = now(), 
    MS * 1000000 + S.

%% +type unixmtime() -> unix_mtime().
%%%% Same, milliseconds.

unixmtime() -> 
    {MS, S, US} = now(), 
    (MS * 1000000 + S) * 1000 + US div 1000.

%% +type datetime_to_unixtime(date_time()) -> unix_time().
%% +type unixtime_to_datetime(unix_time()) -> date_time().
%%%% Note: The constant 62167219200 below is
%%%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})

datetime_to_unixtime(DateTime) ->
    GS = calendar:datetime_to_gregorian_seconds(DateTime),
    GS - 62167219200.

unixtime_to_datetime(T) ->
    calendar:gregorian_seconds_to_datetime(62167219200+T).

local_time_to_universal_time(Local)->
    case calendar:local_time_to_universal_time_dst(Local) of
	[]->
	    Local;
	[Date_UTC|_]->
	    Date_UTC
    end.
%% +type format_unixtime_utc(unix_time()) -> string().
%% +type format_unixtime_loc(unix_time()) -> string().

format_unixtime_utc(T) ->
    T_Universal = unixtime_to_datetime(T),
    format_datetime(T_Universal, " UTC").

format_unixtime_loc(T) ->
    T_Universal = unixtime_to_datetime(T),
    T_Local = calendar:universal_time_to_local_time(T_Universal),
    format_datetime(T_Local, " LOC").

%% +type format_datetime(date_time(), Zone::string()) -> string().
%%%% Formats a date and time according to ISO 8601.

format_datetime({{Y,M,D},{HH,MM,SS}}, Zone) ->
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w~s",
		  [Y,M,D, HH,MM,SS, Zone]).

%% +type get_env(Param::atom()) -> term().
%%%% Returns the value of Param from OMA's environment.
%%%% Exits with an informative message if Param is not declared.

get_env(Param) ->
    application_utils:get_env(oma, Param).
