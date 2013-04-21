%% +deftype unix_time() = integer().
%% +deftype unix_mtime() = integer().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% +deftype param_info() =
%%     #param_info{ type       :: type_expr(),
%%                  name       :: string(),
%%                  help       :: string(),
%%                  example    :: term(),
%%                  activation :: auto_replicated | auto | restart | reboot,
%%                  editor     :: undefined | {Module::atom(),Data},
%%                  level      :: user | install | internal,
%%                  feature    :: atom(),
%%                  related    :: [ atom() | {atom(),atom()} ]
%%                }.
%%%% This record is used by webmin_config.
%%%% Each application should export a function which provides information
%%%% about its configuration parameters:
%%%% Application_app:param_info(atom()) -> param_info().
%%%%
%%%% OMA will use #param_info.type to select an appropriate WWW editor.
%%%% This can be overriden with #param_info.editor.

-record(param_info, {type, name, help, example,
		     activation, editor, level=user, feature, related=[]}).

%% +deftype type_expr() =
%%       bool
%%     | atom
%%     | {opt_atom, Default}
%%     | nodeAtHost
%%     | string
%%     | {string, [{max_length,integer()}]}
%%     | {opt_string, Default}
%%     | int
%%     | {int, [{ge,Val}|{gt,Val}|{le,Val}|{lt,Val}]}
%%     | float
%%     | {float,[Constraint]}
%%     | {enum, [atom()]}
%%     | {tuple, Info::string(), [ field_decl() ]}
%%     | {pairs, atom(), [ {Label::string(),type_expr()} ]}
%%     | {record, Info::string(), atom(), [ field_decl() ]}
%%     | {list, type_expr()}
%%     | {listopts, [atom()|{Value,string()}]}
%%     | {print_raw, type_expr()}         % Directive for webmin
%%     | {printer, PrintFun, type_expr()}   % Directive for webmin
%%     | {defval, Def, type_expr()}       % Directive for webmin
%%     | {editor, Editor, type_expr()}    % Directive for webmin
%%     | {named_type, Name::string, type_expr()}
%%     | {named_type, Name::string, Help::string, type_expr()}
%%     | {pred, fun (Term) -> true|false|ok|{error,E}|exit(E), type_expr()}
%%     | {tagged, [{Tag::atom(),Descr::string(),type_expr()}]}
%%           % Tagged alternatives. Example: {ok,Value} | {error,Value}.
%%     | {alt, [{Tag::atom(),type_expr()}]}
%%           % Generic alternatives. Example: int | {atom,undefined}
%%     | {const, Term}
%%     | void.  % Dangerous

%% +deftype field_decl() =
%%     {Label::string(), type_expr()}
%%   | {Name::atom(), type_expr(), Help::string()}
%%   | {hidden_const,Term}.

%% +deftype env() = [ {App::atom(), [ {Param::atom(),Value} ]} ].
