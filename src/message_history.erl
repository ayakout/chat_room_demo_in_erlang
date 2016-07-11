%%%-------------------------------------------------------------------
%%% @author Ali Yakout <ayakout@Alis-MacBook-Air.local>
%%% @copyright (C) 2015, Ali Yakout
%%% @doc
%%% Message history saves all messages typed in the chat room so that
%%% newly connected clients can read old room messages that were sent
%%% before they join.
%%% The code implements gen_event handler and saves all messages in a
%%% Queue data structure.
%%% @end
%%% Created : 11 Apr 2015 by Ali Yakout <ayakout@Alis-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(message_history).
-behaviour(gen_event).

%% API
-export([add_handler/0, download/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, message_bus).
-record(message, {data, sender}).
-record(state, {message_history}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Download room messages
%%
%% @spec download() -> ok
%% @end
%%--------------------------------------------------------------------
download(ClientPid) ->
    gen_event:notify(?SERVER, {read_history, ClientPid}).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{message_history=queue:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event(#message{}=Event, #state{message_history=MessageHistory}=State) ->
    ct:pal("~p (~p): Add to message queue ~p", [?MODULE, self(), Event]),
    {ok, State#state{message_history=queue:in(Event, MessageHistory)}};
handle_event({read_history, ClientPid}, #state{message_history=MessageHistory}=State) ->
    ct:pal("~p (~p): Request to download messages from ~p", [?MODULE, self(), ClientPid]),
    read_history(MessageHistory, ClientPid),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
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
read_history(MessageHistory, ClientPid) ->
    case queue:out(MessageHistory) of
        {{value, #message{data=Data}}, MessageHistoryRest} ->
            chat_client:incoming_message(ClientPid, Data),
            read_history(MessageHistoryRest, ClientPid);
        {empty, _Empty} ->
            ok
    end.
