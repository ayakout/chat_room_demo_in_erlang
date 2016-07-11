%%%-------------------------------------------------------------------
%%% @author Ali Yakout <ayakout@Alis-MacBook-Air.local>
%%% @copyright (C) 2015, Ali Yakout
%%% @doc
%%% TCP listener process. Loops on gen_tcp:accept/1 and starts a child
%%% process for each connection established to the server.
%%% The server is blocked by a gen_tcp:accept/1 in continous looping.
%%% After each accept operation the server sends itself a message to
%%% start accepting the next connection.
%%% @end
%%% Created : 10 Apr 2015 by Ali Yakout <ayakout@Alis-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(chat_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {listen_socket}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ct:pal("~p (~p): Starting main tcp listener process",
           [?MODULE, self()]),
    Opt = [binary, {packet, 0}, {reuseaddr, true}, {keepalive, true}],
    case gen_tcp:listen(6667, Opt) of
        {ok, ListenSocket} ->
            %% Accept connections as soon as the process initialize
            self() ! accept_connections,
            {ok, #state{listen_socket=ListenSocket}};
        SomeErr ->
            {stop, SomeErr}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(accept_connections, #state{listen_socket=ListenSocket} = State) ->
    ct:pal("~p (~p): Accepting connections at ~p",
           [?MODULE, self(), ListenSocket]),
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            ct:pal("~p (~p): Starting a new client process for ~p!",
                   [?MODULE, self(), Socket]),
            {ok, ClientPid} = chat_client_sup:start_child(Socket),
            gen_tcp:controlling_process(Socket, ClientPid),
            %% Accept tcp again as soon as handle_info returns
            self() ! accept_connections,
            {noreply, State};
        SomeErr ->
            {stop, SomeErr, State}
    end;
handle_info(_Info, State) ->
    ct:pal("~p (~p): Unhandled msg ~p!~n", [?MODULE, self(), _Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{listen_socket=ListenSocket}) ->
    gen_tcp:close(ListenSocket),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
