import gleam/bit_array
import gleam/bytes_tree.{from_string as bits}
import gleam/erlang/atom
import gleam/erlang/process
import gleam/option.{None}
import gleam/string
import gleeunit
import gleeunit/should
import glisten
import glisten/transport
import mug

pub const port = 64_793

pub fn main() {
  // Start an echo TCP server for the tests to use
  let assert Ok(_) =
    glisten.handler(fn(_) { #(Nil, None) }, fn(state, msg, conn) {
      let assert glisten.Packet(msg) = msg
      // Close connection if we receive a null, otherwise echo back.
      let assert Ok(_) = case msg {
        <<0>> -> transport.shutdown(conn.transport, conn.socket)
        _ -> glisten.send(conn, bytes_tree.from_bit_array(msg))
      }

      glisten.continue(state)
    })
    |> glisten.serve(port)

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

  // Allow some time to echo things back, to make the test more reliable
  process.sleep(20)
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
    process.selector_receive(selector, 100)

  packet_socket
  |> should.equal(socket)

  // Send another packet
  let assert Ok(Nil) = mug.send(socket, <<"Hello, Mike!\n":utf8>>)

  // The socket is in passive mode, so we don't get another message.
  let assert Error(Nil) = process.selector_receive(selector, 100)

  // The socket is back in passive mode, we can receive from it directly again.
  let assert Ok(<<"Hello, Mike!\n":utf8>>) = mug.receive(socket, 0)
  let assert Error(mug.Timeout) = mug.receive(socket, 0)
}

pub fn active_mode_close_test() {
  let socket = connect()

  // Ask for the next packet to be sent as a message
  mug.receive_next_packet_as_message(socket)

  // Send a message indicate the socket should be closed
  let assert Ok(Nil) = mug.send(socket, <<0>>)

  let selector =
    process.new_selector()
    |> mug.selecting_tcp_messages(fn(msg) { msg })

  let assert Ok(mug.SocketClosed(closed_socket)) =
    process.selector_receive(selector, 100)

  closed_socket
  |> should.equal(socket)
}

pub fn active_mode_error_test() {
  // Couldn't find a way to trigger an error with an actual TCP server,
  // so we are sending the raw message to ourself instead to mock an error.
  let socket = connect()
  mug.receive_next_packet_as_message(socket)

  let selector =
    process.new_selector()
    |> mug.selecting_tcp_messages(fn(msg) { msg })

  raw_send(process.self(), #(atom.create("tcp_error"), socket, mug.Ehostdown))
  let assert Ok(mug.TcpError(error_socket, mug.Ehostdown)) =
    process.selector_receive(selector, 100)
  error_socket |> should.equal(socket)
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

@external(erlang, "erlang", "send")
fn raw_send(pid: process.Pid, msg: a) -> Nil
