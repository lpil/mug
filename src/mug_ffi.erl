-module(mug_ffi).

-export([send/2, shutdown/1, coerce/1]).

send(Socket, Packet) ->
    normalise(gen_tcp:send(Socket, Packet)).

shutdown(Socket) ->
    normalise(gen_tcp:shutdown(Socket, read_write)).

normalise(ok) -> {ok, nil};
normalise({ok, T}) -> {ok, T};
normalise({error, {timeout, _}}) -> {error, timeout};
normalise({error, _} = E) -> E.

coerce(T) -> T.
