# mug 🍺

[![Package Version](https://img.shields.io/hexpm/v/mug)](https://hex.pm/packages/mug)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/mug/)

A TCP client for Gleam!

```sh
gleam add mug
```

```gleam
import mug

pub fn main() {
  // Form a connection to a TCP server
  let assert Ok(socket) =
    mug.new("erlang-the-movie.example.com", port: 12345)
    |> mug.timeout(milliseconds: 500)
    |> mug.connect()

  // Send a packet to the server
  let assert Ok(Nil) = mug.send(socket, <<"Hello, Joe!\n":utf8>>)

  // Receive a packet back
  let assert Ok(packet) = mug.receive(socket, timeout_milliseconds: 100)
  
  packet
  // -> <<"Hello, Mike!":utf8>>
}
```

It also includes support for receiving packages as Erlang messages, enabling
TCP sockets to be used within OTP actors.

It also has SSL (TLS) support!

```gleam
import mug
import mug/ssl

pub fn main() {
  // Form a connection to a TCP+TLS server
  let assert Ok(socket) =
    mug.new("erlang-the-movie.example.com", port: 12345)
    |> mug.timeout(milliseconds: 500)
    |> ssl.with_ssl()
    |> ssl.connect()

  // Send a packet to the server
  let assert Ok(Nil) = ssl.send(socket, <<"Hello, Joe!\n":utf8>>)

  // Receive a packet back
  let assert Ok(packet) = ssl.receive(socket, timeout_milliseconds: 100)
  
  packet
  // -> <<"Hello, Mike!":utf8>>
}
```

You can also upgrade an existing TCP connection to TLS! This can be useful for
applications which negotiate over plain TCP first, and then upgrade to an
encrypted connection (for example SMTP).

```gleam
import mug
import mug/ssl

pub fn main() {
  // Form a connection to a TCP server
  let assert Ok(tcp_socket) =
    mug.new("example.com", port: 443)  // HTTPS port!
    |> mug.timeout(milliseconds: 500)
    |> mug.connect()

  // Upgrade the connection
  let assert Ok(socket) = ssl.upgrade(tcp_socket)

  // Talk over an encrypted connection!
  let assert Ok(Nil) =
    ssl.send(socket, <<"HEAD / HTTP/1.1\r\nHost: example.com\r\n\r\n":utf8>>)
  let assert Ok(data) = ssl.receive(socket, 5000)
  let assert Ok(data) = bit_array.to_string(data)
  let assert "HTTP/1.1 200 OK\r\n" <> _ = data
}
```

Documentation can be found at <https://hexdocs.pm/mug>.
