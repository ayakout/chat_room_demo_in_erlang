%%%-------------------------------------------------------------------
%%% @author Ali Yakout <ayakout@Alis-MacBook-Air.local>
%%% @copyright (C) 2015, Ali Yakout
%%% @doc
%%% Message relay between chat client processes. The message bus also
%%% keep history os all chat room message and can be downloaded when
%%% new user joins the room.
%%%
%%%                         +---+	       
%%%                         | M |-----------> Client A
%%%                send     | S |
%%%    Client A ----------->+ G +-----------> Client B
%%%    Client B ----------->+   |
%%%              download   | B |-----------> Client C
%%%                         | U |             ...
%%%                         | S |-----------> Client X
%%%                         +---+
%%%
%%% The message bus is implemented as a gen_event manager registered with 
%%% the name 'message_bus' and has 2 types of handlers. One handler is 
%%% for saving the message history (message_history.erl) and multiple 
%%% handlers for each connected client which is responsible for forwarding 
%%% new messages to each client.
%%% @End
%%% Created : 10 Apr 2015 by Ali Yakout <ayakout@Alis-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(message_bus).
-behaviour(gen_event).

%% API
-export([start_link/0, send/1, subscribe/1, unsubscribe/1, download/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {chat_client}).
-record(message, {data, sender}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    Result = gen_event:start_link({local, ?SERVER}),
    message_history:add_handler(),
    Result.

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec subscribe() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
subscribe(ClientPid) ->
    gen_event:add_handler(?SERVER, {?MODULE, ClientPid}, [ClientPid]).

%%--------------------------------------------------------------------
%% @doc
%% Delete an event handler
%%
%% @spec unsubscribe() ->
%%   ok | {error,module_not_found} | {'EXIT',Reason}
%% @end
%%--------------------------------------------------------------------
unsubscribe(ClientPid) ->
    gen_event:delete_handler(?SERVER, {?MODULE, ClientPid}, []).

%%--------------------------------------------------------------------
%% @doc
%% Send a group message
%%
%% @spec send() -> ok
%% @end
%%--------------------------------------------------------------------
send(#message{}=Message) ->
    gen_event:notify(?SERVER, Message).

%%--------------------------------------------------------------------
%% @doc
%% Download room messages
%%
%% @spec download() -> ok
%% @end
%%--------------------------------------------------------------------
download(ClientPid) ->
    message_history:download(ClientPid).

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
init([ClientPid]) ->
    {ok, #state{chat_client=ClientPid}}.

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
%% Skip sending the message to its original sender
%handle_event(#message{sender=ClientPid}, #state{chat_client=ClientPid}=State) ->
%    {ok, State};
handle_event(#message{data=Data}=Event, #state{chat_client=ClientPid}=State) ->
    ct:pal("~p (~p): Notify new message ~p", [?MODULE, self(), Event]),
    chat_client:incoming_message(ClientPid, Data),
    {ok, State};
handle_event(_UnhandledEvent, State) ->
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

