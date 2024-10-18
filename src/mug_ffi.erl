-module(mug_ffi).

-export([connect/4, send/2, shutdown/1, coerce/1]).

connect(Host0, Port0, Opts, Timeout) ->
    {Host1, Port1} = case Host0 of
        "/" ++ _Path -> {{local, Host0}, 0};
        "@" ++ Path -> {{local, [0|Path]}, 0};
        Host0 -> {Host0, Port0}
    end,
    gen_tcp:connect(Host1, Port1, Opts, Timeout).

send(Socket, Packet) ->
    normalise(gen_tcp:send(Socket, Packet)).

shutdown(Socket) ->
    normalise(gen_tcp:shutdown(Socket, read_write)).

normalise(ok) -> {ok, nil};
normalise({ok, T}) -> {ok, T};
normalise({error, {timeout, _}}) -> {error, timeout};
normalise({error, _} = E) -> E.

coerce(T) -> T.
