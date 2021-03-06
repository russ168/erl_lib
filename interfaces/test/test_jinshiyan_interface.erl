-module(test_jinshiyan_interface).

-compile(export_all).

-define(PORT, 54321).

-define(TCP_OPTIONS, [{active, once}]).

test() ->
    application:start(interfaces),
    {ok, Client_socket} = gen_tcp:connect("192.168.196.128", ?PORT, ?TCP_OPTIONS),
    io:fwrite("test_liaoliao_interface ~p:test/0:Client_socket=~p~n",[?LINE, Client_socket]),
    Msg = "[username:jackiedong, password:123, latitude:12345, longitude:54321]",
    ok = gen_tcp:send(Client_socket, Msg),
    receive
        {tcp, Client_socket, Msg} ->
            io:fwrite("test_liaoliao_interface ~p:test/0:Received Msg=~p~n",[?LINE, Msg]),
            gen_tcp:close(Client_socket)
    end.

