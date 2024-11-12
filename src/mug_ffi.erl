-module(mug_ffi).

-export([send/2, shutdown/1, coerce/1, ssl_start/0, ssl_shutdown/1, ssl_send/2]).

send(Socket, Packet) ->
    normalise(gen_tcp:send(Socket, Packet)).

shutdown(Socket) ->
    normalise(gen_tcp:shutdown(Socket, read_write)).

ssl_start() ->
    normalise(ssl:start()).

ssl_shutdown(Socket) ->
    normalise(ssl:shutdown(Socket, read_write)).

ssl_send(Socket, Packet) ->
    normalise(ssl:send(Socket, Packet)).

normalise(ok) -> {ok, nil};
normalise({ok, T}) -> {ok, T};
normalise({error, {timeout, _}}) -> {error, timeout};
normalise({error, _} = E) -> E.

coerce(T) -> T.
