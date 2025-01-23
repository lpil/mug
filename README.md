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
// Make sure you depend on the `ssl` application in `gleam.toml`!
// This can be done by adding `ssl` to the list of `extra_applications` under the
// erlang key in the `gleam.toml` file.
// More info here: https://gleam.run/writing-gleam/gleam-toml/

import mug/tls as mug

pub fn main() {
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

## Testing the library

Some tests testing the TLS capabilities of `mug` require there to be a CA
certificate and a regular certificate and their keys to be present in the
`test/certs` folder. These certs can be generated if you have OpenSSL installed
by running the `./test/certs/gencerts.sh` script from the project's root
directory. If these certificates are not present, you will get a
`Pattern match failed` error on most tests in the `mug_tls_test.gleam` file.

```bash
$ ./test/certs/gencerts.sh  # Run this first if you haven't already!

$ gleam test
```
