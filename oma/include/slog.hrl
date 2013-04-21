%% Delay between slog_flush and slog_sync. Should be enough so that
%% all nodes have finished flushing, but smaller than slog_flush_period.
%% Should also cover clock differences between hosts.
%% Threshold-based alarms can be delayed by this amount.
-define(FLUSH_DELAY, 4000).

%% +deftype range_period() = {second,integer()}
%%                         | {minute,integer()}
%%                         | {hour,  integer()}
%%                         | {day,   integer()}
%%                         | {week,  integer()}.
%%%% Note: We don't support 'month' because intervals must be
%%%% multiples of each other.


%% +deftype slog_row() =
%%     #slog_row{ key :: term(),
%%                op :: slog_count | slog_sum | slog_avg | slog_stats,
%%                ranges :: [ {Current::slog_stats(), Latest::slog_data()} ]
%%              }.
%%%% These statistics records are stored in Mnesia tables.
%%%% 'latest' holds the result of the latest complete range.
%%%% 'current' accumulates data for the current range.

-record(slog_row, {key, op, ranges}).


%% +deftype slog_data() =
%%     #slog_data{ time :: unix_mtime(),  % Latest reset / end of range.
%%                 count :: integer(),    % For 'count', 'avg', 'stats.
%%                 sum :: number(),       % For 'sum', 'avg', 'stats'.
%%                 sum2 :: number(),      % For 'stats'.
%%                 min :: number(),       % For 'stats'.
%%                 max :: number(),       % For 'stats'.
%%                 t_min :: unixtime(),   % For 'stats'.
%%                 t_max :: unixtime(),   % For 'stats'.
%%                 rate :: number(),      % in results for 'count' and 'sum'.
%%                 avg :: number(),       % In results for 'avg' and 'stats'.
%%                 stddev :: number()     % In results for 'stats'.
%%                }.
%%%% Some fields are unused depending on the type of statistics and on
%%%% whether the record in in #slog_row.accs or #slog_row.results.

-record(slog_data, {time, count, sum, sum2,
		    min, max, t_min, t_max,
		    rate, avg, stddev}).

%% +deftype slog_info() =
%%     #slog_info{ descr :: string(),
%%                 operational::string()}.

-record(slog_info, {descr, operational}).

%% +deftype slog_handler() =
%%     #slog_info{ counters         :: dict(),
%%                 last_freq_check  :: unixmtime()
%%                 last_write_check :: unixmtime()}.

-record(slog_handler, {counters, last_freq_check, last_dump_check}).

-define(SLOG_ROW, slog_row).
-define(CALL_HOTLINE,"Call Technical Hotline for further analysis.").
-define(MYSQL_FAILURE,
	"Database may be overloaded or unreachable.\n"
	"1. Check sql interfaces status.\n"
	"2. Check database status by looking at the latest lines of"
	" /var/log/mysqld.log on database nodes.\n"
	" If mysql servers are up and running and that"
	" this problem occurs often call Technical Hotline.").

-define(print_arg(Arg), io_lib:format("~p",[Arg])).

