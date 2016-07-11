%%%-------------------------------------------------------------------
%%% @author Ali Yakout <ayakout@Alis-MacBook-Air.local>
%%% @copyright (C) 2015, Ali Yakout
%%% @doc
%%%
%%% @end
%%% Created : 10 Apr 2015 by Ali Yakout <ayakout@Alis-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(chat_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_child(Socket) ->
    supervisor:start_child(?MODULE, [Socket]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 2000,
    Type = worker,

    AChild = {chat_client, {chat_client, start_link, []},
              Restart, Shutdown, Type, []},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
