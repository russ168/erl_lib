-module(test_udp_jinq).

-compile(export_all).

-define(PORT, 54323).

-define(UDP_OPTIONS, [binary]).

test() ->
    application:start(interfaces),
    {ok, Socket} = gen_udp:open(0, [binary]),
    ok = gen_udp:send(Socket, "localhost", ?PORT, term_to_binary("Hello, udp jinq")),
    Value = receive
                {udp, Socket, Host, Port, Bin} = Msg ->
                    binary_to_term(Bin)
            after 2000 ->
                    0
            end,
    gen_udp:close(Socket),
    Value.

    
