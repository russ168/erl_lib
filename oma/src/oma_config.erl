-module(oma_config).
-export([save_config/0]).
-export([read_config/0]).
-export([reload_config/0, reload/0]).
-export([change_param/3]).
-export([change_config/1]).
-export([merge_envs/1]).
-export([merge_config_files/2]).
-export([write_config/2]).
-export([override_env/2]).
-export([sort_env/1]).

-export([get_config/0]).
-export([filter_params/1]).
-export([compare_configs/2]).
-export([make_diff_config/2]).

-export([string_to_term/1]).

-export([check_ip/1, check_port/1]).
-export([check_type/2]).

-export([default_value/1]).

-export([get_param_info/2]).
-export([check_all_params/0]).

-include_lib("../include/oma.hrl").
-include_lib("../include/slog.hrl").

-export([get_apps/0, read_boot_config/0, read_bootscript_config/0]).
-export([read_sys_config/0,read_base_sys_config/0]).
-export([slog_info/3]).

%% +type save_config() -> ok | {error,Reason}.
%%%% Writes the current local configuration to disk.

save_config() ->
    Env = case application:get_env(oma, save_config_as_diff) of
	      {ok, false} -> get_config();
	      _           -> get_config_diff()
	  end,
    EnvS = io_lib:format("~p.~n", [Env]),
    EnvB = list_to_binary(EnvS),
    Path = "conf/boot.config",
    case file:write_file(Path++".tmp", EnvB) of
	ok -> file:rename(Path++".tmp", Path);
	Error -> Error
    end.

%% Same procedure as release_handler:install_release.

reload() -> reload_config().
    
reload_config() ->
    NewEnv = read_config(),
    net_kernel:monitor_nodes(true),
    OldEnv = application_controller:prep_config_change(),
    Apps = get_apps(),
    _Res1 = application_controller:change_application_data(Apps, NewEnv),
    Res2 = application_controller:config_change(OldEnv),
    slog:event(config, ?MODULE, reload_config, Res2),
    net_kernel:monitor_nodes(false), % TODO Does this affect posmon ?
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_param(App, Param, NewValue) ->
    net_kernel:monitor_nodes(true),
    OldEnv = application_controller:prep_config_change(),
    NewEnv = override_env(OldEnv, [{App, [{Param, NewValue}]}]),

    {value, NewEnv_app} = lists:keysearch(App, 1, NewEnv),
    application_controller:change_application_data(app_desc(App), [NewEnv_app]),

    Res2 = application_controller:config_change(OldEnv),
    slog:event(config, ?MODULE, change_param, {App, Param, Res2}),
    net_kernel:monitor_nodes(false), % TODO Does this affect posmon ?
    ok.

change_config(NewEnv) ->
    OldEnv = application_controller:prep_config_change(),
    Apps = get_apps(),
    Res1_ = application_controller:change_application_data(Apps, NewEnv),
    Res2_ = application_controller:config_change(OldEnv),
    slog:event(config, ?MODULE, change_config, {Res1_, Res2_}),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_config() ->
    %% Parameters from the boot script (they come from the .app files).
    Env_1 = read_boot_config(),
    %% Parameters from the config file (e.g. erl -config orangef.config).
    Env_2 = read_sys_config(),
    Env_12 = override_env(Env_1, Env_2),
    %% Parameters from the command line (e.g. erl -posmon slog_filter []).
    Env_3 = read_cmdline_config(Env_12),
    NewEnv = override_env(Env_12, Env_3),
    NewEnv.

read_base_config() ->
    %% Parameters from the boot script (they come from the .app files).
    Env_1 = read_boot_config(),
    %% Parameters from the config file (e.g. erl -config orangef.config).
    Env_2 = read_base_sys_config(),
    Env_12 = override_env(Env_1, Env_2),
    %% Parameters from the command line (e.g. erl -posmon slog_filter []).
    Env_3 = read_cmdline_config(Env_12),
    NewEnv = override_env(Env_12, Env_3),
    NewEnv.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

app_desc(App) ->
    Apps = get_apps(),
    case lists:keysearch(App, 2, Apps) of
	{value, App_desc} -> [App_desc];
	_ ->
	    %% get_apps/0 return only running applications.
	    %% For this reason, not started App case must be supported.
	    []
    end.

override_env(Env, NewEnv) ->
    lists:foldl(fun override_env_app/2, Env, NewEnv).

override_env_app({AppName,AppEnv}=App, Env) ->
    case lists:keysearch(AppName, 1, Env) of
	{value, {_, CurrentAppEnv}} ->
	    NewAppEnv = lists:foldl(fun override_env_param/2,
				    CurrentAppEnv, AppEnv),
	    lists:keyreplace(AppName, 1, Env, {AppName, NewAppEnv});
	_ -> [App | Env]
    end.
override_env_param({ParamName, _} = Param, AppEnv) ->
    case lists:keysearch(ParamName, 1, AppEnv) of
	{value, {_, _CurrentVal}} ->
	    lists:keyreplace(ParamName, 1, AppEnv, Param);
	_ -> [Param | AppEnv]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Merges environments (e.g. coming from multiple nodes).
%%%% Exits if inconsistencies are detected.
    
merge_envs(Envs) ->
    lists:foldl(fun merge_env/2, [], Envs).

merge_env(Env, NewEnv) ->
    lists:foldl(fun merge_env_app/2, Env, NewEnv).

merge_env_app({AppName,AppEnv}=App, Env) ->
    case lists:keysearch(AppName, 1, Env) of
	{value, {_, CurrentAppEnv}} ->
	    NewAppEnv = lists:foldl(fun (X,T) -> merge_env_param(AppName,X,T)
				    end, CurrentAppEnv, AppEnv),
	    lists:keyreplace(AppName, 1, Env, {AppName,NewAppEnv});
	_ -> [App|Env]
    end.
merge_env_param(AppName, {ParamName,ParamVal}=Param, AppEnv) ->
    case lists:keysearch(ParamName, 1, AppEnv) of
	{value, {_, ParamVal}} -> AppEnv;
	{value, {_, OtherVal}} ->
	    exit({mismatch,AppName,ParamName,ParamVal,OtherVal});
	_ -> [Param|AppEnv]
    end.

merge_config_files(Orig_files, Dest_file) ->
    Envs = [ file:consult(File) || File <- Orig_files ],
    Resulting_env = merge_envs([Env || {ok, [Env]} <- Envs]),
    write_config(Resulting_env, Dest_file).

write_config(Env, File) ->
    Bin_env = list_to_binary(io_lib:format("~p.~n", [Env])),
    file:write_file(File, Bin_env).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flatten1([]) -> [];
flatten1([H|T]) -> H ++ flatten1(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_boot_config() ->
    Apps = get_apps(),
    lists:map(fun ({application,Name,Attrs}) ->
		      Env = case lists:keysearch(env, 1, Attrs) of
				{value, {_,Env_}} -> Env_;
				false             -> []
			    end,
		      {Name, Env}
	      end, Apps).

read_bootscript_config() ->
    {ok, [[BootPath]]} = init:get_argument(boot),
    {ok, [{script, {_Name,_Version}, Commands}]} =
	file:consult(BootPath++".script"),
    lists:foldl(fun env_from_command/2, [], Commands).

env_from_command({apply,{application,load,
			 [{application,AppName,LoadOpts}]}}, Env) ->
    AppEnv = case lists:keysearch(env, 1, LoadOpts) of
		 {value, {env, E}} -> E;
		 false -> []
	     end,
    Env ++ [{AppName,AppEnv}];
env_from_command(_, Env) -> Env.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% +type get_apps() -> [ {application,atom(),[{Key,Value}]} ].
%%%% Returns a list of applications, for use with
%%%% application_controller functions.

get_apps() ->
    %% We must handle at least all running applications because
    %% this is what application_controller:prep_config_change and
    %% application_controller:change_application_data and
    %% application_controller:config_change expect.
    %% We ignore applications which are loading, loaded or starting
    %% because it might be unsafe to change their environment.
    Apps = application:which_applications(),
    lists:map(fun ({AppName,_Desct,_Vsn}) -> read_app_file(AppName) end, Apps).

%% +type read_app_file(atom()) -> {application,atom(),[{Key,Value}]}.
%%%% Reads the ".app" file of an application.  The file must be in
%%%% the load path (this is true if the application is running).

read_app_file(AppName) ->
    AppFileName = atom_to_list(AppName) ++ ".app",
    case file:path_consult(code:get_path(), AppFileName) of
	{ok, [{application,_,_}=App], _} -> App
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Reads "-config <file>"

read_sys_config() ->
    {ok, LLConfigPath} = init:get_argument(config),
    ConfigPaths = flatten1(LLConfigPath),
    lists:foldl(fun (Path, Env) ->
			{ok, [FileEnv]} = file:consult(Path++".config"),
			override_env(Env, FileEnv)
		end, [], ConfigPaths).

%%%% Reads "-config <file>" except conf/boot.

read_base_sys_config() ->
    {ok, LLConfigPath} = init:get_argument(config),
    ConfigPaths = flatten1(LLConfigPath),
    lists:foldl(fun ("conf/boot", Env) -> Env;
		    (Path, Env) ->
			{ok, [FileEnv]} = file:consult(Path++".config"),
			override_env(Env, FileEnv)
		end, [], ConfigPaths).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Reads command-line argument for the applications in Env.

read_cmdline_config(Env) ->
    lists:flatmap(fun ({AppName,_}) ->
			  case read_cmdline_app(AppName) of
			      [] -> [];
			      AppEnv -> [{AppName,AppEnv}]
			  end
		  end, Env).

read_cmdline_app(AppName) ->
    case init:get_argument(AppName) of
	{ok, Args} ->
	    lists:map(fun ([ParamName,ParamVal]) ->
			      {list_to_atom(ParamName),
			       string_to_term(ParamVal)}
		      end, Args);
	_ -> []
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

string_to_term(S) ->
    WhiteToSpace = fun ($\r)->$ ;($\n)->$ ;($\t)->$ ;(C)->C end,
    S1 = lists:map(WhiteToSpace, S),
    {ok, Tokens, _EndLine} = erl_scan:string(S1++"."),
    {ok, F} = erl_parse:parse_term(Tokens),
    F.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_config() ->
    Env = application_controller:prep_config_change(),
    Env1 = filter_params(Env),
    Env2 = sort_env(Env1),
    Env2.

get_config_diff() ->
    BaseEnv = read_base_config(),
    Env = get_config(),
    make_diff_config(sort_env(BaseEnv), sort_env(Env)).

sort_env(Env) ->
    lists:sort(lists:map(fun ({A,E}) -> {A,lists:sort(E)} end, Env)).

%% +type filter_params(env()) -> env().
%%%% Filters out 'included_applications' from an env().

filter_params(Env) ->
    F = fun ({Name,_Value}) -> Name =/= included_applications end,
    lists:map(fun ({App,AppEnv}) -> {App, lists:filter(F, AppEnv)} end, Env).

compare_configs([], []) ->
    [];
compare_configs([{A1,_AE1}|E1], []) ->
    [ {only_left,A1} | compare_configs(E1,[]) ];
compare_configs([], [{A2,_AE2}|E2]) ->
    [ {only_right,A2} | compare_configs([],E2) ];
compare_configs([{A,AE1}|E1], [{A,AE2}|E2]) ->
    compare_configs1(A, AE1, AE2) ++ compare_configs(E1, E2);
compare_configs([{A1,_AE1}|E1], [{A2,_AE2}|_]=E2) when A1<A2 ->
    [ {only_left,A1} | compare_configs(E1,E2) ];
compare_configs([{A1,_AE1}|_]=E1, [{A2,_AE2}|E2]) when A2<A1 ->
    [ {only_right,A2} | compare_configs(E1,E2) ].

compare_configs1(_App, [], []) ->    
    [];
compare_configs1(App, [{P1,_V1}|AE1], []) ->
    [ {only_left,App,P1} | compare_configs1(App,AE1,[]) ];
compare_configs1(App, [], [{P2,_V2}|AE2]) ->
    [ {only_right,App,P2} | compare_configs1(App,[],AE2) ];
compare_configs1(App, [PV|AE1], [PV|AE2]) ->
    compare_configs1(App, AE1, AE2);
compare_configs1(App, [{P,V1}|AE1], [{P,V2}|AE2]) ->
    [ {diff,App,P,V1,V2} | compare_configs1(App, AE1, AE2) ];
compare_configs1(App, [{P1,_V1}|AE1], [{P2,_V2}|_]=AE2) when P1 < P2 ->
    [ {only_left,App,P1} | compare_configs1(App,AE1,AE2) ];
compare_configs1(App, [{P1,_V1}|_]=AE1, [{P2,_V2}|AE2]) when P2 < P1 ->
    [ {only_right,App,P2} | compare_configs1(App,AE1,AE2) ].

%% +type make_diff_config(Env1::env(), Env2::env()) -> Diff::env().
%%%% Generates a diff from Env1 to Env2, i.e. an environment such that
%%%% make_diff_config(sort_env(override_env(Env1,Diff)),Env2)=[].
%%%% Env1 and Env2 must be sorted.
%%%% Diff will be sorted.
    
make_diff_config([], []) ->
    [];
make_diff_config([{_A1,_AE1}|_E1], []) ->
    []; % Cannot remove A1 from E2.
make_diff_config([], [{_A2,_AE2}=App2|_E2]) ->
    mdc_check_empty(App2);
make_diff_config([{A,AE1}|E1], [{A,AE2}|E2]) ->
    mdc_check_empty({A,make_diff_config1(AE1,AE2)})
	++ make_diff_config(E1,E2);
make_diff_config([{A1,_AE1}|E1], [{A2,_AE2}|_]=E2) when A1<A2 ->
    make_diff_config(E1,E2); % Cannot remove A1 from E2.
make_diff_config([{A1,_AE1}|_]=E1, [{A2,_AE2}=App2|E2]) when A2<A1 ->
    mdc_check_empty(App2) ++ make_diff_config(E1,E2).

mdc_check_empty({_, []}) -> [];
mdc_check_empty({_, _}=App) -> [ App ].
		        
make_diff_config1([], []) ->    
    [];
make_diff_config1([{_P1,_V1}|_AE1], []) ->
    []; % Cannot remove P1 from AE2.
make_diff_config1([], [{_P2,_V2}=PV2|AE2]) ->
    [ PV2 | make_diff_config1([],AE2) ];
make_diff_config1([PV|AE1], [PV|AE2]) ->
    make_diff_config1(AE1, AE2);
make_diff_config1([{P,_V1}|AE1], [{P,V2}|AE2]) ->
    [ {P,V2} | make_diff_config1(AE1,AE2) ];
make_diff_config1([{P1,_V1}|AE1], [{P2,_V2}|_]=AE2) when P1 < P2 ->
    make_diff_config1(AE1,AE2); % Cannot remove PV1 from AE2.
make_diff_config1([{P1,_V1}|_]=AE1, [{P2,_V2}=PV2|AE2]) when P2 < P1 ->
    [ PV2 | make_diff_config1(AE1,AE2) ].
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_type(Type, Value) ->
    case checkt(Type, Value) of
	true -> true;
	duplicate -> exit({duplicate_value_for_type, Value, Type});
	false -> exit({invalid_value_for_type, Value, Type})
    end.

checkt(bool, true) -> true;
checkt(bool, false) -> true;
checkt(string, S) -> 
    %% string can be an unicode list
    lists:all(fun (V) -> (V>=0) and is_integer(V) end, S);
checkt({string, Options}, S) ->
    checkt(string, S) and
	begin 
	    F = fun ({max_length, Max}) -> length(S) =< Max;
		    (_) -> true
		end,
	    lists:all(F, Options)
	end;
checkt(int, X) -> is_integer(X);
checkt({print, int}, X) -> is_integer(X);
checkt(opt_int, X) -> is_integer(X);
checkt({opt_int, Undef}, Undef) -> true;
checkt({opt_int, _Undef}, X) when is_integer(X) -> true;
checkt({opt_string, Undef}, Undef) -> true;
checkt({opt_string, _Undef}, X) when is_list(X) -> true;
checkt({int, Options}, X) ->
    checkt(int, X) and
	begin
	    F = fun ({le,M}) -> X =< M;
		    ({lt,M}) -> X < M;
		    ({ge,M}) -> X >= M;
		    ({gt,M}) -> X > M
		end,
	    lists:all(F, Options)
	end;
checkt(float, X) ->
    checkt({float, []}, X);
checkt({float, _Options}, X) ->
    %% Options not implemented yet.
    is_float(X);
checkt(atom, X) -> is_atom(X);
checkt({opt_atom, Undef}, Undef) -> true;
checkt({opt_atom, _Undef}, X) when is_atom(X) -> true;
checkt({enum, Atoms}, X) ->
    case lists:member('$allow_any',Atoms) of
	true->
	    P = fun ({Y,_Label}) -> is_atom(Y); (Y) ->  is_atom(Y) end,
	    lists:any(P, Atoms);
	false->
	    P = fun ({Y,_Label}) -> Y==X; (Y) -> Y==X end,
	    lists:any(P, Atoms)
    end;
checkt(nodeAtHost, X) -> is_atom(X) and lists:member($@, atom_to_list(X));
checkt({pred, Pred, Type}, X) -> check_type(Type, X) and Pred(X);
checkt({print_raw, Type}, X) -> check_type(Type, X);
checkt({printer, _Printer, Type}, X) -> check_type(Type, X);
checkt({defval, _Default, Type}, X) -> check_type(Type, X);
checkt({editor, _Editor, Type}, X) -> check_type(Type, X);
checkt({named_type, _Name, Type}, X) -> check_type(Type, X);
checkt({named_type, _Name, _Help, Type}, X) -> check_type(Type, X);
checkt({list, Type}, X) -> lists:all(fun (I) -> check_type(Type,I) end, X);
checkt({list, Type, {reject_duplicate, {tuple, Pos}}}, X) ->
    Extract = fun(List) -> element(Pos, List) end,
    EList = lists:map(Extract, X),
    case checkt(int, Pos) and (length(EList)==length(lists:usort(EList))) of
	true ->
	    checkt({list, Type}, X);
	_ ->
	    duplicate
    end;
checkt({listopts, List}, X) -> is_subsequence(X, List);
checkt({tuple, _Info, Fields}, X) -> check_fields(Fields, tuple_to_list(X));
checkt({tuple, _Info, Fields,_Help}, X) -> 
    check_fields(Fields, tuple_to_list(X));
checkt({pairs, RecDef, RecFields, Type}, {_RecName,X}) ->
    check_type(Type, oma_util:pairs_to_record(RecDef,RecFields,X));
checkt({record, _Info, Name, Fields}, X) ->
    check_fields([{hidden_const,Name}|Fields], tuple_to_list(X));
checkt({tagged,Subs}, {Tag,Value}) ->
    {value, {_Tag,_Descr,Sub}} = lists:keysearch(Tag, 1, Subs),
    check_type(Sub, Value);
checkt({tagged,Subs}, Tag) when is_atom(Tag) ->
    case lists:keysearch(Tag, 1, Subs) of
	{value, {_,_,void}} -> true;
	_ -> false
    end;
checkt({alt,Subs}, {force_tryalt,Alt,_}) ->
    %%% Is Alt one of the alternative ?
    Alts = lists:map(fun({Elt,_})-> Elt end,Subs),
    lists:member(Alt,Alts);
checkt({alt,Subs}, Value) ->
    F = fun ({_Tag,SubType}) ->
		case catch check_type(SubType, Value) of
		    true -> true;
		    _    -> false
		end
	end,
    lists:any(F, Subs);
checkt({const,C}, X) -> X == C;
checkt(term, _) -> true;
checkt(void, _) -> false;
checkt(_Type, _Value) -> false.

check_fields([], []) ->
    true;
check_fields([{hidden_const,V}|Types], [V|Values]) ->
    check_fields(Types, Values);
check_fields([{_Name,T,_}|Types], [V|Values]) ->
    check_type(T, V) and check_fields(Types, Values);
check_fields([{_Name,T}|Types], [V|Values]) ->
    check_type(T, V) and check_fields(Types, Values).

check_ip(S) -> check_type(string, S).
check_port(P) -> (P>=0) and (P=<65535).

is_subsequence([], _List) -> true;
is_subsequence([Value|Values], [{Value,_Descr}|List]) ->
    is_subsequence(Values, List);
is_subsequence([Value|Values], [Value|List]) when is_atom(Value) ->
    is_subsequence(Values, List);
is_subsequence(Values, [_|List]) ->
    is_subsequence(Values, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

default_value(bool) -> true;
default_value(string) -> "";
default_value({string, _}) -> default_value(string);
default_value(int) -> 0;
default_value({int, _}) -> default_value(int);
default_value({opt_int, Def}) -> Def;
default_value(float) -> 0.0;
default_value({float,_}) -> default_value(float);
default_value({opt_string, Def}) -> Def;
default_value(atom) -> xxxxx;
default_value({opt_atom, Def}) -> Def;
default_value({enum, [{X,_Label}|_]}) -> X;
default_value({enum, [X|_]}) -> X;
default_value(nodeAtHost) -> node();
default_value({pred, _Pred, Type}) -> default_value(Type);
default_value({print_raw, Type}) -> default_value(Type);
default_value({printer, _Printer, Type}) -> default_value(Type);
default_value({defval, Default, _Type}) -> Default;
default_value({editor, _Editor, Type}) -> default_value(Type);
default_value({named_type, _Name, Type}) -> default_value(Type);
default_value({named_type, _Name, _Help, Type}) -> default_value(Type);
default_value({list, _Type}) -> [];
default_value({list, Type, _}) -> default_value({list, Type});
default_value({listopts, _List}) -> [];
default_value({tuple, Info, Fields, _Help}) ->
    default_value({tuple, Info, Fields});
default_value({tuple, _Info, Fields}) ->
    F = fun ({hidden_const,V}) -> V;
	    ({_FieldLabel,FieldType}) -> default_value(FieldType);
	    ({_FieldName,FieldType, _FieldHelp}) -> default_value(FieldType)
	end,
    list_to_tuple(lists:map(F, Fields));
default_value({pairs, RecDef, RecFields, Type}) ->
    RecName = element(1, RecDef),
    DefaultRec = default_value(Type),
    {RecName, oma_util:record_to_pairs(RecFields,DefaultRec)};
default_value({record, Info, RecName, Fields}) ->
    default_value({tuple, Info, [{hidden_const,RecName}|Fields]});
default_value({const, Value}) -> Value;
default_value({tagged, [{Tag,_Descr,void}|_]}) ->
    Tag;
default_value({tagged, [{Tag,_Descr,Type}|_]}) ->
    {Tag, default_value(Type)};
default_value({alt, [{_Tag,Type}|_]}) ->
    default_value(Type);
default_value(term) -> any;
default_value(Type) ->
    slog:event(warning, ?MODULE, no_default_value, Type),
    undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% +type get_param_info(App::atom(), Param::atom()) -> param_info().

get_param_info(App, Param) ->
    Module =
	%% Use {mod,AppMod}.  Fallback to MODULE_app for libraries.
	case application:get_key(App, mod) of
	    {ok, {Module_, _Args}} -> Module_;
	    _                    -> list_to_atom(atom_to_list(App)++"_app")
	end,
    case catch apply(Module, param_info, [Param]) of
	{'EXIT', {undef, [{Module,param_info,_}|_]}} ->
	    %% get param for OTP application
	    get_otp_param_info(App, Param);
	{'EXIT', {function_clause, [{Module,param_info,_}|_]}} ->
	    exit(no_help);
	{'EXIT', Error} ->
	    exit(Error);
	Result ->
	    Result
    end.
%% type get_otp_param_info(App::atom(), Param::atom()) -> param_info().
get_otp_param_info(App, Param)->
    Module = list_to_atom(atom_to_list(App)++"_info"),
    %% if necessary, use a config parameter to construct module.
    case catch apply(Module, param_info, [Param]) of
	{'EXIT', {undef, [{Module,param_info,_}|_]}} ->
	    exit(no_help);
	{'EXIT', {function_clause, [{Module,param_info,_}|_]}} ->
	    exit(no_help);
	{'EXIT', Error} ->
	    exit(Error);
	Result ->
	    Result
    end.
    
    
check_all_params() ->
    io:format("OMA: Checking configuration parameters...~n"),
    put(oma_config_errors, false),
    Env = get_config(),
    lists:foreach
      (fun ({App,AppEnv}) ->
	       lists:foreach
		 (fun ({Param,Value}) ->
			  check_param_type(App,Param,Value)
		  end, AppEnv)
       end, Env),
    get(oma_config_errors).

check_param_type(App, Param, Value) ->
    case catch get_param_info(App, Param) of
	#param_info{type=Type} ->
	    case catch check_type(Type, Value) of
		true  -> ignore;
		Error -> put(oma_config_errors, true),
			 io:format("  ## ~p.~p  ~100p~n", [App,Param,Error])
	    end;
	{'EXIT', {no_app_module,_}} -> ignore;
	{'EXIT', {undef,_}} -> ignore;
	{'EXIT', {function_clause,_}} ->
	    io:format("  ?? No type declaration for ~p.~p~n", [App, Param]);
	Error ->
	    put(oma_config_errors, true),
	    io:format("  ## ~p.~p error ~100p~n", [App, Param, Error])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% slog_info
%% +type slog_info(C::atom(),M::atom(),N::term())-> slog_info().
slog_info(config, ?MODULE, reload_config)->
    #slog_info{descr="System configuration was reloaded from disk."};

slog_info(config, ?MODULE, change_param) ->
    #slog_info{descr="A configuration parameter was modified."};

slog_info(config, ?MODULE, change_config) ->
    #slog_info{descr="System Configuration was modified."};

slog_info(warning, ?MODULE, no_default_value) ->
    #slog_info{descr="No default value could be found for a parameter type."
               " It can happen for :\n"
               " - configuration parameters: in this case, an event"
               " {config, webmin_config, param_edit, {Application, Parameter}}"
               " is generated in the logs just before this warning.\n"
               " - any other part of the application using the oma_webmin"
               " framework. In this case, run/webmin/logs/access_log can be"
               " used to correlate this event with an URL using the timestamp"
              }. 
