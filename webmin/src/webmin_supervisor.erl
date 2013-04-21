%%%-------------------------------------------------------------------
%%% File    : webmin_supervisor.erl
%%% Author  :  <>
%%% Description : 
%%%
%%% Created : 28 Feb 2011 by  <>
%%%-------------------------------------------------------------------
-module(webmin_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link([]) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    AChild = {webmin_httpd,{webmin_httpd,start_link,[]},
              permanent,2000,worker,[webmin_httpd]},
    {ok,{{one_for_all,0,1}, [AChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
