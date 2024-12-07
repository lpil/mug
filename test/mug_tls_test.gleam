import gleam/bit_array
import gleam/bytes_tree.{from_string as bits}
import gleam/erlang/process
import gleam/string
import gleeunit/should
import mug
import mug/tls

pub const port = 64_794

fn connect() {
  let assert Ok(_) = tls.start()
  let assert Ok(socket) =
    tls.new("localhost", port: port)
    |> tls.dangerously_disable_verification()
    |> tls.connect()
  socket
}

pub fn connect_without_verification_test() {
  let socket = connect()
  let assert Ok(_) = tls.shutdown(socket)
  Nil
}

pub fn connect_with_system_ca_test() {
  let assert Ok(_) = tls.start()
  let assert Ok(socket) =
    tls.new("example.com", port: 443)
    |> tls.timeout(milliseconds: 10_000)
    |> tls.connect()
  let assert Ok(_) = tls.shutdown(socket)
  Nil
}

pub fn connect_without_system_ca_test() {
  let assert Ok(_) = tls.start()
  let assert Error(tls.TlsAlert(tls.UnknownCa, _)) =
    tls.new("gleam.run", port: 443)
    |> tls.no_system_cacerts()
    |> tls.timeout(milliseconds: 10_000)
    |> tls.connect()
  Nil
}

pub fn connect_invalid_host_test() {
  let assert Ok(_) = tls.start()
  let assert Error(tls.Nxdomain) =
    tls.new("invalid.example.com", port: port)
    |> tls.timeout(milliseconds: 500)
    |> tls.connect()
}

pub fn upgrade_test() {
  let assert Ok(tcp_socket) =
    mug.new("localhost", port: port)
    |> mug.connect()
  let assert Ok(_) = tls.start()
  let assert Ok(socket) =
    tls.upgrade(tcp_socket, tls.DangerouslyDisableVerification, 1000)
  let assert Ok(Nil) = tls.send(socket, <<"Hello, Joe!\n":utf8>>)
  let assert Ok(data) = tls.receive(socket, 500)
  should.equal(data, <<"Hello, Joe!\n":utf8>>)
  let assert Ok(_) = tls.shutdown(socket)
  Nil
}

pub fn upgrade_with_system_ca_test() {
  let assert Ok(tcp_socket) =
    mug.new("example.com", port: 443)
    |> mug.connect()
  let assert Ok(_) = tls.start()
  let assert Ok(socket) =
    tls.upgrade(tcp_socket, tls.DangerouslyDisableVerification, 5000)
  let assert Ok(Nil) =
    tls.send(socket, <<"HEAD / HTTP/1.1\r\nHost: example.com\r\n\r\n":utf8>>)
  let assert Ok(data) = tls.receive(socket, 5000)
  let assert Ok(data) = bit_array.to_string(data)
  let assert "HTTP/1.1 200 OK\r\n" <> _ = data
  let assert Ok(_) = tls.shutdown(socket)
  Nil
}

pub fn hello_world_test() {
  let socket = connect()

  // Nothing has been sent by the echo server yet, so we get a timeout if we try
  // to receive a packet.
  let assert Error(tls.Timeout) = tls.receive(socket, timeout_milliseconds: 0)

  let assert Ok(Nil) = tls.send(socket, <<"Hello, Joe!\n":utf8>>)
  let assert Ok(Nil) = tls.send(socket, <<"Hello, Mike!\n":utf8>>)
  let assert Ok(Nil) = tls.send_builder(socket, bits("System still working?\n"))
  let assert Ok(Nil) = tls.send_builder(socket, bits("Seems to be!"))

  let assert Ok(packet) = tls.receive(socket, timeout_milliseconds: 100)
  let assert Ok(packet) = bit_array.to_string(packet)
  string.split(packet, "\n")
  |> should.equal([
    "Hello, Joe!", "Hello, Mike!", "System still working?", "Seems to be!",
  ])

  let assert Ok(_) = tls.shutdown(socket)

  // if this sleep call does not exist, the below command *sometimes* errors out.
  process.sleep(1)
  let assert Error(tls.Closed) = tls.send(socket, <<"One more thing!":utf8>>)
  // the below statement times out if timeout_milliseconds is 0, instead of closing
  // the connection. Probably because of the internal workings of the SSL library.
  let assert Error(tls.Closed) = tls.receive(socket, timeout_milliseconds: 1)
}

pub fn active_mode_test() {
  let socket = connect()

  // Ask for the next packet to be sent as a message
  tls.receive_next_packet_as_message(socket)

  // The socket is in use, we can't receive from it directly
  let assert Error(tls.Einval) = tls.receive(socket, 0)

  // Send a message to the socket
  let assert Ok(Nil) = tls.send(socket, <<"Hello, Joe!\n":utf8>>)

  let selector =
    process.new_selector()
    |> tls.selecting_tcp_messages(fn(msg) { msg })

  let assert Ok(tls.Packet(packet_socket, <<"Hello, Joe!\n":utf8>>)) =
    process.select(selector, 100)

  packet_socket
  |> should.equal(socket)

  // Send another packet
  let assert Ok(Nil) = tls.send(socket, <<"Hello, Mike!\n":utf8>>)

  // The socket is in passive mode, so we don't get another message.
  let assert Error(Nil) = process.select(selector, 100)

  // The socket is back in passive mode, we can receive from it directly again.
  let assert Ok(<<"Hello, Mike!\n":utf8>>) = tls.receive(socket, 0)
  let assert Error(tls.Timeout) = tls.receive(socket, 0)
}

pub fn exact_bytes_receive_test() {
  let socket = connect()

  let assert Ok(Nil) = tls.send(socket, <<"Hello":utf8>>)
  let assert Ok(Nil) = tls.send(socket, <<"World":utf8>>)

  let assert Ok(<<"Hello":utf8>>) = tls.receive_exact(socket, 5, 100)
  let assert Ok(<<"World":utf8>>) = tls.receive_exact(socket, 5, 100)

  let assert Ok(_) = tls.shutdown(socket)

  let assert Error(tls.Closed) = tls.receive_exact(socket, 5, 100)
}

pub fn exact_bytes_receive_not_enough_test() {
  let socket = connect()

  let assert Ok(Nil) = tls.send(socket, <<"Hello":utf8>>)
  let assert Ok(Nil) = tls.send(socket, <<"Worl":utf8>>)

  let assert Ok(<<"Hello":utf8>>) = tls.receive_exact(socket, 5, 100)
  let assert Error(tls.Timeout) = tls.receive_exact(socket, 5, 100)

  let assert Ok(_) = tls.shutdown(socket)

  let assert Error(tls.Closed) = tls.receive_exact(socket, 5, 100)
}
