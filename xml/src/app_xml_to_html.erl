-module(app_xml_to_html).

-compile(export_all).

%% import(xmerl_xs,
%%         [ xslapply/2, value_of/1, select/2, built_in_rules/2 ]).

%% -include_lib("xmerl/include/xmerl.hrl").

%% to_html(App_XML_file) ->
%%     {Parsed, _} = xmerl_scan:file(App_XML_file),
%%     template(Parsed).
   
%% %%% templates
%% template(E = #xmlElement{name=apidoc}) ->
%%     xslapply(fun template/1, select("//function", E));
%% template(E = #xmlElement{name='function'}) ->
%%     [    xslapply(fun template/1, select("@name", E)),
%%          xslapply(fun template/1, select("param/@name", E)),
%%          xslapply(fun template/1, select("result/@type", E)),
%%          value_of(select("comment", E)),
%%          "####################################"
%%         ];
%% template(E) -> built_in_rules(fun template/1, E).
