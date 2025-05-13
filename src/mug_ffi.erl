-module(mug_ffi).

-export([send/2, recv/3, shutdown/1, coerce/1, ssl_upgrade/3, ssl_connect/4, get_certs_keys/1, ssl_downgrade/2, get_system_cacerts/0]).

send({tcp_socket, Socket}, Packet) ->
    normalise(gen_tcp:send(Socket, Packet));
send({ssl_socket, Socket}, Packet) ->
		normalise(ssl:send(Socket, Packet)).

recv({tcp_socket, Socket}, Length, Timeout) ->
    gen_tcp:recv(Socket, Length, Timeout);
recv({ssl_socket, Socket}, Length, Timeout) ->
		ssl:recv(Socket, Length, Timeout).

shutdown({tcp_socket, Socket}) ->
    normalise(gen_tcp:shutdown(Socket, read_write));
shutdown({ssl_socket, Socket}) ->
    normalise(ssl:shutdown(Socket, read_write)).

ssl_upgrade(Socket, Options, Timeout) ->
    normalise(ssl:connect(Socket, Options, Timeout)).

ssl_connect(Host, Port, Options, Timeout) ->
    normalise(ssl:connect(Host, Port, Options, Timeout)).

get_certs_keys(CertsKeysList) ->
    lists:map(fun (CertsKeys) -> case CertsKeys of
        {der_encoded_certificates_keys, Certs, {der_encoded_key, KeyAlg, KeyBin}} ->
            #{ cert => lists:map(fun unicode:characters_to_list/1, Certs), key => {normalize_key_algo(KeyAlg), KeyBin} };
        {pem_encoded_certificates_keys, Certfile, Keyfile, none} ->
            #{ certfile => unicode:characters_to_list(Certfile), keyfile => unicode:characters_to_list(Keyfile) };
        {pem_encoded_certificates_keys, Certfile, Keyfile, {some, Password}} ->
            #{ certfile => unicode:characters_to_list(Certfile), keyfile => unicode:characters_to_list(Keyfile), password => unicode:characters_to_list(Password) }
    end end, CertsKeysList).

normalize_key_algo(rsa_private_key) -> 'RSAPrivateKey';
normalize_key_algo(dsa_private_key) -> 'DSAPrivateKey';
normalize_key_algo(ec_private_key) -> 'ECPrivateKey';
normalize_key_algo(private_key_info) -> 'PrivateKeyInfo'.

ssl_downgrade({tcp_socket, _}, _) -> {error, timeout};
ssl_downgrade({ssl_socket, Socket}, Timeout) ->
    case ssl:close(Socket, Timeout) of
        ok -> {error, closed};
        {ok, Port} -> {ok, {Port, nil}};
        {ok, Port, Data} -> {ok, {Port, {some, Data}}};
        {error, _} = E -> E
    end.

get_system_cacerts() ->
    try public_key:cacerts_get() of
        Certs -> {ok, Certs}
    catch
        error:{failed_load_certs, Reason} -> {error, Reason}
    end.

normalise(ok) -> {ok, nil};
normalise({ok, T}) -> {ok, T};
normalise({error, {timeout, _}}) -> {error, timeout};
normalise({error, {tls_alert, {Alert, Description}}}) -> {error, {tls_alert, Alert, list_to_binary(Description)}};
normalise({error, _} = E) -> E.

coerce(T) -> T.
