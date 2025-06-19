-module(mug_ffi).

-export([send/2, shutdown/1, coerce_tcp_message/1, active_once/0, passive/0]).

active_once() ->
    once.

passive() ->
    false.

send(Socket, Packet) ->
    normalise(gen_tcp:send(Socket, Packet)).

shutdown(Socket) ->
    normalise(gen_tcp:shutdown(Socket, read_write)).

normalise(ok) ->
    {ok, nil};
normalise({ok, T}) ->
    {ok, T};
normalise({error, {timeout, _}}) ->
    {error, timeout};
normalise({error, _} = E) ->
    E.

coerce_tcp_message({tcp, Socket, Data}) ->
    {packet, Socket, Data};
coerce_tcp_message({tcp_closed, Socket}) ->
    {socket_closed, Socket};
coerce_tcp_message({tcp_error, Socket, Error}) ->
    {tcp_error, Socket, Error}.
