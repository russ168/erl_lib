%% Colors for HTML tables.
-define(COLOR_CAPTION,   "cyan").
-define(COLOR_DISABLED,  "white").
-define(COLOR_UP,        "#C0FFC0").
-define(COLOR_WARNING,   "#FFD0B0"). % Orange
-define(COLOR_DOWN,      "#FFC0C0"). % Red
-define(COLOR_HIGHLIGHT, "#FFFFC0"). % Yellow

%% oma Style CSS
-define(oma_css,"/oma/oma_style.css").
%% webmin alarm classes in a table
-define(accepted, "accepted").
-define(critical, "critical").
-define(major,"major").
-define(warning,"warning").
-define(minor,"minor").
-define(indeterminate,"indeterminate").
-define(deactivated,"deactivated").

%% text event classes
-define(down,"down").
-define(ready,"ready").
-define(up,"up").
-define(disabled,"disabled").
-define(highlight,"highlight").

%% Symbols (for text-only browsers).
-define(TAG_DISABLED,  "").
-define(TAG_UP,        "").
-define(TAG_WARNING,   "?").
-define(TAG_DOWN,      "!").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% +deftype html() = string().
%% +deftype esi_result() = string().
%% +deftype http_env() = [ {atom(),string()} ].

%%%% For webmin_callback.
%% +deftype http_callback() =
%%     fun (http_env(), [{string(),string()}]) -> esi_result().

%% +defbehaviour webmin_editor(Option, Value) = begin
%%   +type print(Option, Value) -> html().
%%   +type edit(Option, Value, fun (Value) -> esi_result()) ->
%% 	   { html(),
%% 	     fun (http_env(),[{Var::string(),Val::string()}]) -> {ok,Value}
%% 	   }.
%% end.

