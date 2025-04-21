-module(mug_ffi).

-export([connect_network/4, connect_socket/3, send/2, shutdown/1, coerce/1]).

connect_network(Host, Port, Opts, Timeout) ->
    gen_tcp:connect(Host, Port, Opts, Timeout).

connect_socket(Host0, Opts, Timeout) ->
    Host1 = case Host0 of
        "@" ++ Path -> {local, [0|Path]};
        _ -> {local, Host0}
    end,
    gen_tcp:connect(Host1, 0, Opts, Timeout).


send(Socket, Packet) ->
    normalise(gen_tcp:send(Socket, Packet)).

shutdown(Socket) ->
    normalise(gen_tcp:shutdown(Socket, read_write)).

normalise(ok) -> {ok, nil};
normalise({ok, T}) -> {ok, T};
normalise({error, {timeout, _}}) -> {error, timeout};
normalise({error, _} = E) -> E.

coerce(T) -> T.
