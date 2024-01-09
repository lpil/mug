# mug ☕

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

Documentation can be found at <https://hexdocs.pm/mug>.
