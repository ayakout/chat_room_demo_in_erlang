#!/usr/bin/env escript
-export([main/1]).
-export([receive_loop/1]).

main([]) ->
    ct:pal("Connecting to chatroom@localhost:6667 ..."),
    {ok, Socket} = gen_tcp:connect("localhost", 6667, [binary, {packet, 0}, {active, false}]),
    spawn_link(fun()-> receive_loop(Socket) end),
    send_loop(Socket).

send_loop(Socket) ->
    Input = io:get_line("chatroom> "),
    gen_tcp:send(Socket, Input),
    send_loop(Socket).

receive_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("\r~s~n", [Data]),
            receive_loop(Socket);
        _SomeErr ->
            ok
    end.
