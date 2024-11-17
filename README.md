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
import mug/tls as mug

pub fn main() {
  // Either depend on the `ssl` application in `gleam.toml`, or run:
  mug.start()
  // Form a connection to a TCP+TLS server
  let assert Ok(socket) =
    mug.new("example.com", port: 443)  // HTTPS port!
    |> mug.timeout(milliseconds: 5000)
    |> mug.connect()

  // Talk over an encrypted connection!
  let assert Ok(Nil) =
    mug.send(socket, <<"HEAD / HTTP/1.1\r\nHost: example.com\r\n\r\n":utf8>>)
  let assert Ok(data) = mug.receive(socket, 5000)
  let assert Ok(data) = bit_array.to_string(data)
  let assert "HTTP/1.1 200 OK\r\n" <> _ = data
}
```

You can also upgrade an existing TCP connection to TLS! This can be useful for
applications which negotiate over plain TCP first, and then upgrade to an
encrypted connection (for example SMTP).

```gleam
import gleam/option.{Some}
import mug
import mug/tls

pub fn main() {
  // Form a connection to a TCP server
  let assert Ok(tcp_socket) =
    mug.new("erlang-the-movie-2.example.com", port: 12345)
    |> mug.timeout(milliseconds: 500)
    |> mug.connect()

  // Talk over plain text!
  let assert Ok(Nil) = mug.send(socket, <<"Let's upgrade!\n":utf8>>)
  let assert Ok(_) = mug.receive(socket, timeout_milliseconds: 100)

  // Now upgrade the connection
  let assert Ok(socket) = tls.upgrade(
    tcp_socket,
    // Do not check certificates (Do not use in prod!!)
    tls.NoVerification,
    // Timeout after 1000 millis
    milliseconds: 1000
  )

  // Talk over an encrypted connection!
  let assert Ok(Nil) =
    tls.send(socket, <<"Hello, Joe!\n":utf8>>)
  let assert Ok(_) = tls.receive(socket, 5000)

  // Downgrade to an unencrypted connection
  case tls.downgrade(socket, milliseconds: 1000) {
    Error(tls.Closed) -> {
      // Downgrade failed, so the connection was closed.
      Nil
    }
    Ok(#(tcp_socket, data)) -> {
      // `data` is the first piece of data received (if any)
      // after downgrade
      let assert Some(_) = data
      let assert Ok(Nil) = mug.send(socket, <<"Scary unencrypted connection!\n":utf8>>)
    }
    Error(_) -> {
      // Downgrade failed for other reasons
      Nil
    }
  }
}
```

Documentation can be found at <https://hexdocs.pm/mug>.
