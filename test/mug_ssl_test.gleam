import gleam/bit_array
import gleam/bytes_builder.{from_string as bits}
import gleam/erlang/process
import gleam/option
import gleam/string
import gleeunit/should
import mug
import mug/ssl

pub const port = 64_794

fn connect() {
  let assert Ok(_) = ssl.start()
  let assert Ok(socket) =
    ssl.new("localhost", port: port)
    |> ssl.no_verification()
    |> ssl.connect()
  socket
}

pub fn connect_without_ca_test() {
  let socket = connect()
  let assert Ok(_) = ssl.shutdown(socket)
  Nil
}

// FIXME!!
pub fn connect_with_custom_ca_test() {
  let assert Ok(_) = ssl.start()
  let assert Ok(socket) =
    ssl.new("localhost", port: port)
    |> ssl.cacerts(ssl.PemEncodedCaCertificates("./test/certs/ca.crt"))
    |> ssl.connect()
  let assert Ok(_) = ssl.shutdown(socket)
}

// FIXME!!
pub fn connect_with_certs_keys_test() {
  let assert Ok(_) = ssl.start()
  let assert Ok(socket) =
    ssl.new("localhost", port)
    |> ssl.certs_keys([
      ssl.PemEncodedCertsKeys(
        "./test/certs/server.crt",
        "./test/certs/server.key",
        option.None,
      ),
    ])
    |> ssl.connect()
  let assert Ok(_) = ssl.shutdown(socket)
}

pub fn connect_with_system_ca_test() {
  let assert Ok(_) = ssl.start()
  let assert Ok(socket) =
    ssl.new("example.com", port: 443)
    |> ssl.timeout(milliseconds: 10_000)
    |> ssl.connect()
  let assert Ok(_) = ssl.shutdown(socket)
  Nil
}

pub fn connect_invalid_host_test() {
  let assert Ok(_) = ssl.start()
  let assert Error(ssl.Nxdomain) =
    ssl.new("invalid.example.com", port: port)
    |> ssl.timeout(milliseconds: 500)
    |> ssl.connect()
}

pub fn upgrade_test() {
  let assert Ok(tcp_socket) =
    mug.new("localhost", port: port)
    |> mug.connect()
  let assert Ok(_) = ssl.start()
  let assert Ok(socket) = ssl.upgrade(tcp_socket, ssl.NoVerification, 1000)
  let assert Ok(Nil) = ssl.send(socket, <<"Hello, Joe!\n":utf8>>)
  let assert Ok(data) = ssl.receive(socket, 500)
  should.equal(data, <<"Hello, Joe!\n":utf8>>)
  let assert Ok(_) = ssl.shutdown(socket)
  Nil
}

pub fn upgrade_with_system_ca_test() {
  let assert Ok(tcp_socket) =
    mug.new("example.com", port: 443)
    |> mug.connect()
  let assert Ok(_) = ssl.start()
  let assert Ok(socket) = ssl.upgrade(tcp_socket, ssl.NoVerification, 5000)
  let assert Ok(Nil) =
    ssl.send(socket, <<"HEAD / HTTP/1.1\r\nHost: example.com\r\n\r\n":utf8>>)
  let assert Ok(data) = ssl.receive(socket, 5000)
  let assert Ok(data) = bit_array.to_string(data)
  let assert "HTTP/1.1 200 OK\r\n" <> _ = data
  let assert Ok(_) = ssl.shutdown(socket)
  Nil
}

pub fn hello_world_test() {
  let socket = connect()

  // Nothing has been sent by the echo server yet, so we get a timeout if we try
  // to receive a packet.
  let assert Error(ssl.Timeout) = ssl.receive(socket, timeout_milliseconds: 0)

  let assert Ok(Nil) = ssl.send(socket, <<"Hello, Joe!\n":utf8>>)
  let assert Ok(Nil) = ssl.send(socket, <<"Hello, Mike!\n":utf8>>)
  let assert Ok(Nil) = ssl.send_builder(socket, bits("System still working?\n"))
  let assert Ok(Nil) = ssl.send_builder(socket, bits("Seems to be!"))

  let assert Ok(packet) = ssl.receive(socket, timeout_milliseconds: 100)
  let assert Ok(packet) = bit_array.to_string(packet)
  string.split(packet, "\n")
  |> should.equal([
    "Hello, Joe!", "Hello, Mike!", "System still working?", "Seems to be!",
  ])

  let assert Ok(_) = ssl.shutdown(socket)

  // if this sleep call does not exist, the below command *sometimes* errors out.
  process.sleep(1)
  let assert Error(ssl.Closed) = ssl.send(socket, <<"One more thing!":utf8>>)
  // the below statement times out if timeout_milliseconds is 0, instead of closing
  // the connection. Probably because of the internal workings of the SSL library.
  let assert Error(ssl.Closed) = ssl.receive(socket, timeout_milliseconds: 1)
}

pub fn active_mode_test() {
  let socket = connect()

  // Ask for the next packet to be sent as a message
  ssl.receive_next_packet_as_message(socket)

  // The socket is in use, we can't receive from it directly
  let assert Error(ssl.Einval) = ssl.receive(socket, 0)

  // Send a message to the socket
  let assert Ok(Nil) = ssl.send(socket, <<"Hello, Joe!\n":utf8>>)

  let selector =
    process.new_selector()
    |> ssl.selecting_tcp_messages(fn(msg) { msg })

  let assert Ok(ssl.Packet(packet_socket, <<"Hello, Joe!\n":utf8>>)) =
    process.select(selector, 100)

  packet_socket
  |> should.equal(socket)

  // Send another packet
  let assert Ok(Nil) = ssl.send(socket, <<"Hello, Mike!\n":utf8>>)

  // The socket is in passive mode, so we don't get another message.
  let assert Error(Nil) = process.select(selector, 100)

  // The socket is back in passive mode, we can receive from it directly again.
  let assert Ok(<<"Hello, Mike!\n":utf8>>) = ssl.receive(socket, 0)
  let assert Error(ssl.Timeout) = ssl.receive(socket, 0)
}

pub fn exact_bytes_receive_test() {
  let socket = connect()

  let assert Ok(Nil) = ssl.send(socket, <<"Hello":utf8>>)
  let assert Ok(Nil) = ssl.send(socket, <<"World":utf8>>)

  let assert Ok(<<"Hello":utf8>>) = ssl.receive_exact(socket, 5, 100)
  let assert Ok(<<"World":utf8>>) = ssl.receive_exact(socket, 5, 100)

  let assert Ok(_) = ssl.shutdown(socket)

  let assert Error(ssl.Closed) = ssl.receive_exact(socket, 5, 100)
}

pub fn exact_bytes_receive_not_enough_test() {
  let socket = connect()

  let assert Ok(Nil) = ssl.send(socket, <<"Hello":utf8>>)
  let assert Ok(Nil) = ssl.send(socket, <<"Worl":utf8>>)

  let assert Ok(<<"Hello":utf8>>) = ssl.receive_exact(socket, 5, 100)
  let assert Error(ssl.Timeout) = ssl.receive_exact(socket, 5, 100)

  let assert Ok(_) = ssl.shutdown(socket)

  let assert Error(ssl.Closed) = ssl.receive_exact(socket, 5, 100)
}
