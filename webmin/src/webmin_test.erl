-module(webmin_test).

-compile(export_all).

show(Env, Para) ->
    io:fwrite("webmin_test ~p:show/2:Para=~p~n",[?LINE, Para]),
    io:fwrite("webmin_test ~p:show/2:Env=~p~n",[?LINE, Env]),
    "<html>
    <head>
    </head>
    <body>
    " ++ Env ++ 
    "</body>
     </html>".
