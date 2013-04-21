-module(udp_jinq).

-compile(export_all).

-include ("../../mongodb/include/mongo_protocol.hrl").

-define(PORT, 54323).

-define(UDP_OPTIONS, [binary]).

start_link() ->
    Pid = spawn_link(fun init/0),
    {ok, Pid}.
    

init() ->
    case gen_udp:open(?PORT, ?UDP_OPTIONS) of
        {ok, Socket} -> 
            loop(Socket);
        Error ->
            io:fwrite("udp_jinq ~p:init/0:Error=~p~n",[?LINE, Error])
    end.

loop(Socket) ->
    receive
        {udp, Socket, Host, Port, Bin} = Msg ->
            io:fwrite("udp_jinq ~p:loop/1:Msg=~p~n",[?LINE, Msg]),
            {ok,{obj,List},[]} = rfc4627:decode(Bin),
            io:fwrite("udp_jinq ~p:loop/1:List=~p~n",[?LINE, List]),
            Bson = to_bson(List),
            io:fwrite("udp_jinq ~p:loop/1:Bson=~p~n",[?LINE, Bson]),

            %% write Bson to mongodb
            {ok, Conn} = mongo:connect ("127.0.0.1"),
            DbConn = {test, Conn},
            Res1 = mongo_query:write (DbConn, #insert {collection = lbs, documents = [Bson]}, {}),
            io:fwrite("udp_jinq ~p:loop/1:Res1=~p~n",[?LINE, Res1]),

            %% search 
            Cursor = mongo_query:find (DbConn, #'query' {collection = lbs, selector = {}}),
            Res2 = mongo_cursor:rest (Cursor),
            io:fwrite("udp_jinq ~p:loop/1:Res2=~p~n",[?LINE, Res2]),
            mongo_cursor:close (Cursor),
            
            %% Response to the client
            Response = rfc4627:encode({obj, [{name, hideto}, {age, 23}]}),   
            io:fwrite("udp_jinq ~p:loop/1:Response=~p~n",[?LINE, Response]),
            gen_udp:send(Socket, Host, Port, Response),
            loop(Socket)
    end.

to_bson(PropList) ->
    Fun = fun({N1, N2}, AccIn) ->
                  [N2, list_to_atom(N1)|AccIn]
          end,
    L = lists:reverse(lists:foldl(Fun, [], PropList)),
    list_to_tuple(L).

