import gleam/bit_builder.{from_string as bits}
import gleam/bit_string
import gleam/otp/actor
import gleam/string
import gleeunit
import gleeunit/should
import glisten
import glisten/acceptor
import glisten/handler
import glisten/tcp
import mug

pub const port = 64_793

pub fn main() {
  // Start an echo TCP server for the tests to use
  let assert Ok(_) =
    handler.func(fn(msg, state) {
      let assert Ok(_) =
        tcp.send(state.socket, bit_builder.from_bit_string(msg))
      actor.continue(state)
    })
    |> acceptor.new_pool
    |> glisten.serve(port, _)

  gleeunit.main()
}

pub fn hello_world_test() {
  let assert Ok(socket) =
    mug.new("localhost", port: port)
    |> mug.timeout(milliseconds: 500)
    |> mug.connect()

  // Nothing has been sent by the echo server yet, so we get a timeout if we try
  // to receive a packet.
  let assert Error(mug.Timeout) = mug.receive(socket, timeout_milliseconds: 0)

  let assert Ok(Nil) = mug.send(socket, <<"Hello, Joe!\n":utf8>>)
  let assert Ok(Nil) = mug.send(socket, <<"Hello, Mike!\n":utf8>>)
  let assert Ok(Nil) = mug.send_builder(socket, bits("System still working?\n"))
  let assert Ok(Nil) = mug.send_builder(socket, bits("Seems to be!"))

  let assert Ok(packet) = mug.receive(socket, timeout_milliseconds: 100)
  let assert Ok(packet) = bit_string.to_string(packet)
  string.split(packet, "\n")
  |> should.equal([
    "Hello, Joe!", "Hello, Mike!", "System still working?", "Seems to be!",
  ])

  let assert Ok(_) = mug.shutdown(socket)

  let assert Error(mug.Closed) = mug.send(socket, <<"One more thing!":utf8>>)
  let assert Error(mug.Closed) = mug.receive(socket, timeout_milliseconds: 0)
}
