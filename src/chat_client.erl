%%%-------------------------------------------------------------------
%%% @author Ali Yakout <ayakout@Alis-MacBook-Air.local>
%%% @copyright (C) 2015, Ali Yakout
%%% @doc
%%% Client process responsible for reading user messages and forward the
%%% messages to the message bus.
%%% Send announcement on joining/leaving the chat annd download the chat
%%% history at startup.
%%% @end
%%% Created : 10 Apr 2015 by Ali Yakout <ayakout@Alis-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(chat_client).

-behaviour(gen_fsm).

%% API
-export([start_link/1]).
-export([incoming_message/2]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).
-define(FILTER_OUT, "$\\"). %% Emacs highlight issue"

-record(state, {socket}).
-record(message, {data, sender}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Socket) ->
    gen_fsm:start_link(?MODULE, [Socket], []).

%%--------------------------------------------------------------------
%% @doc
%% Sends an event asynchronously to the gen_fsm FsmRef and returns
%% ok immediately. The gen_fsm will call Module:handle_event/3 to
%% handle the event.
%%
%% @spec incoming_message() -> ok
%% @end
%%--------------------------------------------------------------------
incoming_message(Pid, Data) ->
    gen_fsm:send_all_state_event(Pid, {incoming_msg, Data}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Socket]) ->
    ct:pal("~p (~p): Client process started for ~p",
           [?MODULE, self(), Socket]),
    message_bus:subscribe(self()),
    message_bus:download(self()),
    message_bus:send(#message{data=join_message(), sender=self()}),
    {ok, state_name, #state{socket=Socket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
state_name(_Event, State) ->
    {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event({incoming_msg, Data}, StateName, #state{socket=Socket}=State) ->
    ct:pal("~p (~p): Incoming message ~p", [?MODULE, self(), Data]),
    gen_tcp:send(Socket, Data),
    %% DeliveryReport ??
    {next_state, StateName, State};
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Socket, <<?FILTER_OUT, _/binary>> = Data}, StateName,
            #state{socket=Socket}=State) ->
    ct:pal("~p (~p): User message filtered out ~p", [?MODULE, self(), Data]),
    {next_state, StateName, State};
handle_info({tcp, Socket, Data}, StateName, #state{socket=Socket}=State) ->
    ct:pal("~p (~p): User sent chat message ~p", [?MODULE, self(), Data]),
    message_bus:send(#message{data=format_message(Data), sender=self()}),
    {next_state, StateName, State};
handle_info({tcp_closed, Socket}, _StateName, #state{socket=Socket}=State) ->
    ct:pal("~p (~p): Client disconnected at ~p", [?MODULE, self(), Socket]),
    {stop, tcp_closed, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, #state{socket=Socket}) ->
    message_bus:send(#message{data=leave_message(), sender=self()}),
    gen_tcp:close(Socket),
    message_bus:unsubscribe(self()),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
format_message(Data) ->
    io_lib:format("~p - ~s~n ~s", [self(), httpd_util:rfc1123_date(), Data]).
join_message() ->
    io_lib:format("PING! - ~p~n ~p has joined the room~n",
                  [httpd_util:rfc1123_date(), self()]).
leave_message() ->
    io_lib:format("PING! - ~p~n ~p has left the room~n",
                  [httpd_util:rfc1123_date(), self()]).
