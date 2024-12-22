import gleam/bit_array
import gleam/bytes_tree.{from_string as bits}
import gleam/erlang/process
import gleam/io
import gleam/string
import gleeunit/should
import mug

pub const port = 64_794

fn connect() {
  let assert Ok(mug.SslSocket(socket)) =
    mug.new("localhost", port: port)
    |> mug.with_tls()
    |> mug.dangerously_disable_verification()
    |> mug.connect()
  mug.SslSocket(socket)
}

pub fn connect_without_verification_test() {
  let socket = connect()
  let assert Ok(_) = mug.shutdown(socket)
  Nil
}

pub fn connect_with_system_ca_test() {
  let assert Ok(socket) =
    mug.new("example.com", port: 443)
    |> mug.timeout(milliseconds: 10_000)
    |> mug.with_tls()
    |> mug.connect()
  let assert Ok(_) = mug.shutdown(socket)
  Nil
}

pub fn connect_without_system_ca_test() {
  let assert Error(mug.TlsAlert(mug.UnknownCa, _)) =
    mug.new("gleam.run", port: 443)
    |> mug.timeout(milliseconds: 10_000)
    |> mug.with_tls()
    |> mug.no_system_cacerts()
    |> mug.connect()
  Nil
}

pub fn connect_invalid_host_test() {
  let assert Error(mug.Nxdomain) =
    mug.new("invalid.example.com", port: port)
    |> mug.timeout(milliseconds: 500)
    |> mug.with_tls()
    |> mug.connect()
}

pub fn upgrade_test() {
  let assert Ok(tcp_socket) =
    mug.new("localhost", port: port)
    |> mug.connect()
  let assert Ok(mug.SslSocket(socket)) =
    mug.upgrade(tcp_socket, mug.DangerouslyDisableVerification, 1000)
  let socket = mug.SslSocket(socket)
  let assert Ok(Nil) = mug.send(socket, <<"Hello, Joe!\n":utf8>>)
  let assert Ok(data) = mug.receive(socket, 500)
  should.equal(data, <<"Hello, Joe!\n":utf8>>)
  let assert Ok(_) = mug.shutdown(socket)
  Nil
}

pub fn upgrade_with_system_ca_test() {
  let assert Ok(tcp_socket) =
    mug.new("example.com", port: 443)
    |> mug.connect()
  let assert Ok(mug.SslSocket(socket)) =
    mug.upgrade(tcp_socket, mug.DangerouslyDisableVerification, 5000)
  let socket = mug.SslSocket(socket)
  let assert Ok(Nil) =
    mug.send(socket, <<"HEAD / HTTP/1.1\r\nHost: example.com\r\n\r\n":utf8>>)
  let assert Ok(data) = mug.receive(socket, 5000)
  let assert Ok(data) = bit_array.to_string(data)
  let assert "HTTP/1.1 200 OK\r\n" <> _ = data
  let assert Ok(_) = mug.shutdown(socket)
  Nil
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

  // if this sleep call does not exist, the below command *sometimes* errors out.
  process.sleep(1)
  let assert Error(mug.Closed) = mug.send(socket, <<"One more thing!":utf8>>)
  // the below statement times out if timeout_milliseconds is 0, instead of closing
  // the connection. Probably because of the internal workings of the SSL library.
  let assert Error(mug.Closed) = mug.receive(socket, timeout_milliseconds: 1)
}

pub fn active_mode_test() {
  let socket = connect()

  process.flush_messages()

  // Ask for the next packet to be sent as a message
  mug.receive_next_packet_as_message(socket)

  // The socket is in use, we can't receive from it directly
  let assert Error(mug.Einval) = mug.receive(socket, 0)

  // Send a message to the socket
  let assert Ok(Nil) = mug.send(socket, <<"Hello, Joe!\n":utf8>>)

  let selector =
    process.new_selector()
    |> mug.selecting_tls_messages(fn(msg) { msg })

  let assert Ok(mug.Packet(packet_socket, <<"Hello, Joe!\n":utf8>>)) =
    process.select(selector, 1000)
    |> io.debug()

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
