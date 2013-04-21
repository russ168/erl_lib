%%%-------------------------------------------------------------------
%%% File    : jinshiyan_interface.erl
%%% Author  :  <>
%%% Description : 
%%%
%%% Created :  7 Mar 2011 by  <>
%%%-------------------------------------------------------------------
-module(jinshiyan_interface).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(PORT, 54321).

-define(TCP_OPTIONS, [{active, once}]).

-record(state, {listen_socket}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
   {ok, Listen} = gen_tcp:listen(?PORT, ?TCP_OPTIONS),
    io:fwrite("jinshiyan_interface ~p:init/1:Listen=~p~n",[?LINE, Listen]),
   spawn(fun loop_listen/1, [Listen]),
   {ok, #state{listen_socket = Listen}}.

loop_listen(ListenSocket) ->
    io:fwrite("jinshiyan_interface ~p:loop_listen/1:ListenSocket=~p~n",[?LINE, ListenSocket]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> loop_listen(ListenSocket) end),
    loop(Socket).

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

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{listen_socket = Listen}) ->
    gen_tcp:close(Listen),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
