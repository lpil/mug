import gleam/bytes_builder.{type BytesBuilder}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process
import gleam/io
import gleam/result
import mug.{type ConnectionOptions, type Error}

pub type Socket

type DoNotLeak

pub type SslConnectionOptions {
  SslConnectionOptions(
    host: String,
    port: Int,
    timeout: Int,
    cacerts: CaCertificates,
  )
}

/// The CA certificates to use
pub type CaCertificates {
  /// Do not verify certificates.
  NoVerification
  /// Use these der-encoded certificates as CA certificates. This will make mug not use
  /// the system's CA certificates, but only these ones. To use the system's certificates
  /// and custom ones, use the `WithSystemCertificatesAnd` variant instead.
  CustomDerCertificates(cacerts: List(BitArray))
  /// Path to a pem-encoded file which contains CA certificates. This will make mug not use
  /// the system's CA certificates, but only these ones. To use the system's certificates
  /// and custom ones, use the `WithSystemCertificatesAnd` variant instead.
  CustomPemCertificates(cacertfile: String)
  /// Use the system's CA certificates, as provided by erlang's `cacerts_get/0` function
  /// from the `public_key` module ([docs](https://www.erlang.org/doc/apps/public_key/public_key#cacerts_get/0)).
  SystemCertificates
  /// Use the system's certificates, along with `cacerts` more, which is a list of
  /// der-encoded CA certificates as BitArrays. A pem-file cannot be used with this option
  /// because der-encoded certs have a higher priority over pem-encoded cert files, and the
  /// `cacerts_get/0` function returns der-encoded certs.
  WithSystemCertificatesAnd(cacerts: List(BitArray))
}

pub fn with_ssl(options: ConnectionOptions) -> SSLConnectionOptions {
  let mug.ConnectionOptions(host, port, timeout) = options
  SSLConnectionOptions(host, port, timeout, cacerts: SystemCertificates)
}

pub fn with_cacerts(
  options: SSLConnectionOptions,
  cacerts: CaCertificates,
) -> SSLConnectionOptions {
  SSLConnectionOptions(..options, cacerts: cacerts)
}

// TODO: Support certs_keys option of [common_option_cert](https://www.erlang.org/doc/apps/ssl/ssl.html#t:common_option_cert/0).
fn with_certs_keys() {
  todo as "not implemented"
}

/// Establish a TLS-encrypted TCP connection to the server specified in the
/// connection options.
///
/// Returns an error if the connection could not be established.
///
/// The socket is created in passive mode, meaning the the `receive` function is
/// to be called to receive packets from the client. The
/// `receive_next_packet_as_message` function can be used to switch the socket
/// to active mode and receive the next packet as an Erlang message.
///
pub fn connect(options: SSLConnectionOptions) -> Result(Socket, Error) {
  let host = charlist.from_string(options.host)
  use _ <- result.try(ssl_start())
  ssl_connect(
    host,
    options.port,
    get_tls_options(options.cacerts),
    options.timeout,
  )
}

/// Upgrade a plain TCP connection to TLS.
///
/// Accepts a TCP socket and performs the client-side TLS handshake. This
/// may not work on all TCP servers. It uses the system's CA certificates
/// to perform the handshake by default, and has a default timeout of 1000.
/// To customise both of these things, use `upgrade3`.
///
/// Returns an error if the connection could not be established.
///
/// The socket is created in passive mode, meaning the the `receive` function is
/// to be called to receive packets from the client. The
/// `receive_next_packet_as_message` function can be used to switch the socket
/// to active mode and receive the next packet as an Erlang message.
///
pub fn upgrade(socket: mug.Socket) {
  upgrade3(socket, SystemCertificates, 1000)
}

/// Upgrade a plain TCP connection to TLS.
///
/// Accepts a TCP socket and performs the client-side TLS handshake. This
/// may not work on all TCP servers.
///
/// Returns an error if the connection could not be established.
///
/// The socket is created in passive mode, meaning the the `receive` function is
/// to be called to receive packets from the client. The
/// `receive_next_packet_as_message` function can be used to switch the socket
/// to active mode and receive the next packet as an Erlang message.
///
pub fn upgrade3(
  socket: mug.Socket,
  cacerts: CaCertificates,
  timeout: Int,
) -> Result(Socket, Error) {
  use _ <- result.try(ssl_start())
  ssl_upgrade(socket, get_tls_options(cacerts), timeout)
}

fn get_tls_options(cacerts: CaCertificates) -> List(#(TlsOptionName, Dynamic)) {
  [
    // When data is received on the socket queue it in the TCP stack rather than
    // sending it as an Erlang message to the socket owner's inbox.
    #(Active, dynamic.from(False)),
    // We want the data from the socket as bit arrays please, not lists.
    #(Mode, dynamic.from(Binary)),
    #(
      Verify,
      dynamic.from(case cacerts {
        NoVerification -> VerifyNone
        _ -> VerifyPeer
      }),
    ),
    ..get_cacerts(cacerts)
  ]
}

fn get_cacerts(cacerts: CaCertificates) -> List(TlsOption) {
  case cacerts {
    NoVerification -> []
    CustomDerCertificates(cacerts) -> [#(Cacerts, dynamic.from(cacerts))]
    CustomPemCertificates(cacerts) -> [#(Cacertfile, dynamic.from(cacerts))]
    SystemCertificates -> [#(Cacerts, dynamic.from(get_system_cacerts()))]
    WithSystemCertificatesAnd(cacerts) -> [
      #(Cacerts, dynamic.from([get_system_cacerts().0, ..cacerts])),
    ]
  }
}

/// Adapted from https://www.erlang.org/doc/apps/public_key/public_key#t:combined_cert/0
type CombinedCert =
  #(BitArray, #(Dynamic, Dynamic, Dynamic))

@external(erlang, "public_key", "cacerts_get")
fn get_system_cacerts() -> CombinedCert

type VerifyOption {
  VerifyPeer
  VerifyNone
}

type TlsOptionName {
  // gen_tcp options
  Active
  Mode
  // `client_option`s
  Verify
  Cacerts
  Cacertfile
}

type ModeValue {
  Binary
}

type ActiveValue {
  Once
}

type TlsOption =
  #(TlsOptionName, Dynamic)

@external(erlang, "mug_ffi", "ssl_start")
fn ssl_start() -> Result(Nil, Error)

@external(erlang, "ssl", "connect")
fn ssl_connect(
  host: Charlist,
  port: Int,
  options: List(TlsOption),
  timeout: Int,
) -> Result(Socket, Error)

@external(erlang, "ssl", "connect")
fn ssl_upgrade(
  socket: mug.Socket,
  options: List(TlsOption),
  timeout: Int,
) -> Result(Socket, Error)

/// Send a packet to the client.
///
pub fn send(socket: Socket, packet: BitArray) -> Result(Nil, Error) {
  send_builder(socket, bytes_builder.from_bit_array(packet))
}

/// Send a packet to the client, the data in `BytesBuilder`. Using this function
/// is more efficient than turning a `BytesBuilder` or a `StringBuilder` into a
/// `BitArray` to use with the `send` function.
///
@external(erlang, "mug_ffi", "ssl_send")
pub fn send_builder(socket: Socket, packet: BytesBuilder) -> Result(Nil, Error)

/// Receive a packet from the client.
///
/// Errors if the socket is closed, if the timeout is reached, or if there is
/// some other problem receiving the packet.
///
pub fn receive(
  socket: Socket,
  timeout_milliseconds timeout: Int,
) -> Result(BitArray, Error) {
  ssl_receive(socket, 0, timeout_milliseconds: timeout)
}

/// Receive the specified number of bytes from the client, unless the socket
/// was closed, from the other side. In that case, the last read may return
/// less bytes.
/// If the specified number of bytes is not available to read from the socket
/// then the function will block until the bytes are available, or until the
/// timeout is reached.
/// This directly calls the underlying Erlang function `ssl:recv/3`.
///
/// Errors if the socket is closed, if the timeout is reached, or if there is
/// some other problem receiving the packet.
pub fn receive_exact(
  socket: Socket,
  byte_size size: Int,
  timeout_milliseconds timeout: Int,
) -> Result(BitArray, Error) {
  ssl_receive(socket, size, timeout_milliseconds: timeout)
}

@external(erlang, "ssl", "recv")
fn ssl_receive(
  socket: Socket,
  read_bytes_num: Int,
  timeout_milliseconds timeout: Int,
) -> Result(BitArray, Error)

/// Close the socket, ensuring that any data buffered in the socket is flushed to the operating system kernel socket first.
///
@external(erlang, "mug_ffi", "ssl_shutdown")
pub fn shutdown(socket: Socket) -> Result(Nil, Error)

/// Switch the socket to active mode, meaning that the next packet received on
/// the socket will be sent as an Erlang message to the socket owner's inbox.
///
/// This is useful for when you wish to have an OTP actor handle incoming
/// messages as using the `receive` function would result in the actor being
/// blocked and unable to handle other messages while waiting for the next
/// packet.
///
/// Messages will be send to the process that controls the socket, which is the
/// process that established the socket with the `connect` function.
///
pub fn receive_next_packet_as_message(socket: Socket) -> Nil {
  set_socket_options(socket, [#(Active, dynamic.from(Once))])
  |> io.debug()
  Nil
}

@external(erlang, "ssl", "setopts")
fn set_socket_options(socket: Socket, options: List(TlsOption)) -> DoNotLeak

/// Messages that can be sent by the socket to the process that controls it.
///
pub type SslMessage {
  /// A packet has been received from the client.
  Packet(Socket, BitArray)
  /// The socket has been closed by the client.
  SocketClosed(Socket)
  /// An error has occurred on the socket.
  TcpError(Socket, Error)
}

/// Configure a selector to receive messages from TCP sockets.
///
/// Note this will receive messages from all TCP sockets that the process
/// controls, rather than any specific one. If you wish to only handle messages
/// from one socket then use one process per socket.
///
pub fn selecting_tcp_messages(
  selector: process.Selector(t),
  mapper: fn(SslMessage) -> t,
) -> process.Selector(t) {
  let ssl = atom.create_from_string("ssl")
  let closed = atom.create_from_string("ssl_closed")
  let error = atom.create_from_string("ssl_error")

  selector
  |> process.selecting_record3(ssl, unsafe_coerce_packet(mapper))
  |> process.selecting_record2(closed, unsafe_coerce_closed(mapper))
  |> process.selecting_record3(error, unsafe_coerce_to_tcp_error(mapper))
}

fn unsafe_coerce_packet(
  mapper: fn(SslMessage) -> t,
) -> fn(Dynamic, Dynamic) -> t {
  fn(socket, data) {
    Packet(unsafe_coerce(socket), unsafe_coerce(data))
    |> mapper
  }
}

fn unsafe_coerce_closed(mapper: fn(SslMessage) -> t) -> fn(Dynamic) -> t {
  fn(socket) {
    SocketClosed(unsafe_coerce(socket))
    |> mapper
  }
}

fn unsafe_coerce_to_tcp_error(
  mapper: fn(SslMessage) -> t,
) -> fn(Dynamic, Dynamic) -> t {
  fn(socket, reason) {
    mapper(TcpError(unsafe_coerce(socket), unsafe_coerce(reason)))
  }
}

@external(erlang, "mug_ffi", "coerce")
fn unsafe_coerce(data: Dynamic) -> a
