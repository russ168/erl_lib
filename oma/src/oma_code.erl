-module(oma_code).
-compile(export_all).

-include_lib("kernel/include/file.hrl").

%% Expect no more than 10s discrepancy between the compile time
%% attribute in beam file and the unix file modification time.
-define(TIMESTAMP_ACCURACY, 10).

%% +type check_reload() ->
%%     {Ignore::[atom()], Unchanged::[atom()], 
%%      NeedUpgrade::[atom()], NeedDowngrade::[atom()]}.

check_reload() ->
    Mods = code:all_loaded(),
    F = fun ({Mod,Path}, {Ign,Unc,Up,Dn,Del}) ->
		case check_reload(Mod, Path) of
                    ignore    -> {[Mod|Ign],Unc,Up,Dn,Del};
                    unchanged -> {Ign,[Mod|Unc],Up,Dn,Del};
                    upgrade   -> {Ign,Unc,[Mod|Up],Dn,Del};
                    downgrade -> {Ign,Unc,Up,[Mod|Dn],Del};
                    deleted   -> {Ign,Unc,Up,Dn,[Mod|Del]}
		end
	end,
    lists:foldl(F, {[],[],[],[],[]}, Mods).

%% +type check_reload(Module::atom(), Path :: string()|preloaded) ->
%%     ignore | unchanged | upgrade | downgrade.

check_reload(Module, Path) when is_list(Path) ->
    Compile_info = Module:module_info(compile),
    {value, {time, Loaded}} = lists:keysearch(time, 1, Compile_info),
    case compile_time(Path) of
        {file_error, _, enoent} ->
            deleted;
        Beam ->
            [Loaded_s, Beam_s] = [to_seconds(T) || T <- [Loaded, Beam]],
            if Beam_s > Loaded_s + ?TIMESTAMP_ACCURACY -> upgrade;
               Beam_s < Loaded_s - ?TIMESTAMP_ACCURACY -> downgrade;
               true -> unchanged
            end
    end;

check_reload(_, _) ->
    ignore.

%% +type compile_time(Beam_or_binary :: string()|binary()) ->
%%     {Year::integer(), Month::integer(), Day::integer(),
%%      Hours::integer(), Minutes::integer(), Seconds::integer()}.
%%%% Returns the compilation time of a beam file (or binary) without loading it.

compile_time(Beam_or_binary) ->
    case beam_lib:chunks(Beam_or_binary,["CInf"]) of
        {ok, {_, [{"CInf", Chunk}]}} ->
            Compile_info = binary_to_term(Chunk),
            {value, {time, Result}} =
                lists:keysearch(time,1, Compile_info),
            Result;
        {error, beam_lib, Error} ->
            Error
    end.

%% +type to_seconds({Year::integer(), Month::integer(), Day::integer(),
%%      Hours::integer(), Minutes::integer(), Seconds::integer()}) ->
%%     unix_time().

to_seconds({Y, M, D, H, Min, S}) ->
    oma:datetime_to_unixtime({{Y, M, D}, {H, Min, S}}).

%% +type reload() ->
%%     {Ignored::[atom()], Unchanged::[atom()], 
%%      Upgraded::[atom()], Downgraded::[atom()]}.

reload() ->
    reload(check_reload()).

%% +type reload({Ignore::[atom()], Unchanged::[atom()], 
%%               NeedUpgrade::[atom()], NeedDowngrade::[atom()]}) ->
%%     {Ignored::[atom()], Unchanged::[atom()], 
%%      Upgraded::[atom()], Downgraded::[atom()]}.

reload({_,_,Up,Dn,Del}=Mods) ->
    Reload = fun (M) -> code:purge(M), code:load_file(M) end,
    Delete = fun (M) -> code:delete(M), code:purge(M) end,
    lists:map(Reload, Up++Dn),
    lists:map(Delete, Del),
    Mods.

%% +type patched_modules() -> {Mod::atom(),Vsn::term()}.
%%%% Returns the list of modules with an explicit '-vsn("...")' attribute.
%%%% By convention these are the patched modules.
%%%% Only the modules which are currently loaded are checked.

patched_modules() ->
    CheckMod = fun ({Mod, _CodePath}) ->
		       Attrs = Mod:module_info(attributes),
		       %% Note: We ignore multiple -vsn attributes.
		       case lists:keysearch(vsn, 1, Attrs) of
			   {value, {vsn,[{patch,V}]}} -> [{Mod,V}];
			   _ -> []
		       end
	       end,
    Mods = code:all_loaded(),
    lists:flatmap(CheckMod, Mods).

%% +deftype fun_beam() =
%%   fun(Name::string(), Module::atom(), Acc::term()) -> term().
%% +type fold_beams(Dir::string(), Recursive::bool(), fun_beam(), Acc::term())
%%           -> term().
fold_beams(Dir, Recursive, Fun, Acc_in) ->
    filelib:fold_files(
      Dir,
      "\.beam$",
      Recursive,
      fun(File, Acc) ->
	      Module = list_to_atom(filename:basename(File, ".beam")),
	      Fun(File, Module, Acc)
      end,
      Acc_in).

