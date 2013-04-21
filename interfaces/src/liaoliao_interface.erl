-module(liaoliao_interface).

-compile(export_all).

-define(PORT, 54322).

-define(TCP_OPTIONS, [{active, once}]).

start_link() ->
    Pid = proc_lib:spawn_link(?MODULE, init, []),
    {ok, Pid}.

init() ->
    case gen_tcp:listen(?PORT, ?TCP_OPTIONS) of
        {ok, Listen} -> 
            loopAccept(Listen);
        Error ->
            io:fwrite("liaoliao_interface ~p:par_connect/1:Error=~p~n",[?LINE, Error])
    end.

loopAccept(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            Pid = spawn(fun loop/1, [Socket]),
            gen_tcp:controlling_process(Socket, Pid),
            loopAccept(Listen);
        Error ->
            io:fwrite("liaoliao_interface ~p:loopAccept/1:Error=~p~n",[?LINE, Error])
            
    end.

loop(Socket) ->
    receive
        {tcp, Socket, Data} ->
            io:fwrite("liaoliao ~p:loop/1:Data=~p~n",[?LINE, Data]),
            gen_tcp:send(Socket, Data),
            inet:setopts(Socket, ?TCP_OPTIONS),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:fwrite("liaoliao ~p:loop/1:Socket=~p Closed ~n",[?LINE, Socket]),
            gen_tcp:close(Socket)
    end.
