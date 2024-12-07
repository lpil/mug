import gleam/bit_array
import gleam/bytes_tree.{from_string as bits}
import gleam/erlang/process
import gleam/option.{None}
import gleam/otp/actor
import gleam/string
import gleeunit
import gleeunit/should
import glisten
import mug
import mug_tls_test.{port as ssl_port}

pub const port = 64_793

pub fn main() {
  // Start an echo TCP server for the tests to use
  let handler =
    glisten.handler(fn(_) { #(Nil, None) }, fn(msg, state, conn) {
      let assert glisten.Packet(msg) = msg
      let assert Ok(_) = glisten.send(conn, bytes_tree.from_bit_array(msg))
      actor.continue(state)
    })
  let assert Ok(_) = glisten.serve(handler, port)
  let assert Ok(_) =
    glisten.serve_ssl(
      handler,
      ssl_port,
      certfile: "test/certs/server.crt",
      keyfile: "test/certs/server.key",
    )

  gleeunit.main()
}

fn connect() -> mug.Socket {
  let assert Ok(socket) =
    mug.new("localhost", port: port)
    |> mug.timeout(milliseconds: 500)
    |> mug.connect()
  socket
}

pub fn connect_invalid_host_test() {
  let assert Error(mug.Nxdomain) =
    mug.new("invalid.example.com", port: port)
    |> mug.timeout(milliseconds: 500)
    |> mug.connect()
}

pub fn hello_world_test() {
  let socket = connect()

  // Nothing has been sent by the echo server yet, so we get a timeout if we try
  // to receive a packet.
  let assert Error(mug.Timeout) = mug.receive(socket, timeout_milliseconds: 0)

  let assert Ok(Nil) = mug.send(socket, <<"Hello, Joe!\n":utf8>>)
  let assert Ok(Nil) = mug.send(socket, <<"Hello, Mike!\n":utf8>>)
  let assert Ok(Nil) = mug.send_builder(socket, bits("System still working?\n"))
  let assert Ok(Nil) = mug.send_builder(socket, bits("Seems to be!"))

  let assert Ok(packet) = mug.receive(socket, timeout_milliseconds: 100)
  let assert Ok(packet) = bit_array.to_string(packet)
  string.split(packet, "\n")
  |> should.equal([
    "Hello, Joe!", "Hello, Mike!", "System still working?", "Seems to be!",
  ])

  let assert Ok(_) = mug.shutdown(socket)

  let assert Error(mug.Closed) = mug.send(socket, <<"One more thing!":utf8>>)
  let assert Error(mug.Closed) = mug.receive(socket, timeout_milliseconds: 0)
}

pub fn active_mode_test() {
  let socket = connect()

  // Ask for the next packet to be sent as a message
  mug.receive_next_packet_as_message(socket)

  // The socket is in use, we can't receive from it directly
  let assert Error(mug.Einval) = mug.receive(socket, 0)

  // Send a message to the socket
  let assert Ok(Nil) = mug.send(socket, <<"Hello, Joe!\n":utf8>>)

  let selector =
    process.new_selector()
    |> mug.selecting_tcp_messages(fn(msg) { msg })

  let assert Ok(mug.Packet(packet_socket, <<"Hello, Joe!\n":utf8>>)) =
    process.select(selector, 100)

  packet_socket
  |> should.equal(socket)

  // Send another packet
  let assert Ok(Nil) = mug.send(socket, <<"Hello, Mike!\n":utf8>>)

  // The socket is in passive mode, so we don't get another message.
  let assert Error(Nil) = process.select(selector, 100)

  // The socket is back in passive mode, we can receive from it directly again.
  let assert Ok(<<"Hello, Mike!\n":utf8>>) = mug.receive(socket, 0)
  let assert Error(mug.Timeout) = mug.receive(socket, 0)
}

pub fn exact_bytes_receive_test() {
  let socket = connect()

  let assert Ok(Nil) = mug.send(socket, <<"Hello":utf8>>)
  let assert Ok(Nil) = mug.send(socket, <<"World":utf8>>)

  let assert Ok(<<"Hello":utf8>>) = mug.receive_exact(socket, 5, 100)
  let assert Ok(<<"World":utf8>>) = mug.receive_exact(socket, 5, 100)

  let assert Ok(_) = mug.shutdown(socket)

  let assert Error(mug.Closed) = mug.receive_exact(socket, 5, 100)
}

pub fn exact_bytes_receive_not_enough_test() {
  let socket = connect()

  let assert Ok(Nil) = mug.send(socket, <<"Hello":utf8>>)
  let assert Ok(Nil) = mug.send(socket, <<"Worl":utf8>>)

  let assert Ok(<<"Hello":utf8>>) = mug.receive_exact(socket, 5, 100)
  let assert Error(mug.Timeout) = mug.receive_exact(socket, 5, 100)

  let assert Ok(_) = mug.shutdown(socket)

  let assert Error(mug.Closed) = mug.receive_exact(socket, 5, 100)
}
