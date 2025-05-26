import gleam/bytes_tree.{type BytesTree}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process
import gleam/result

/// A TCP socket, used to send and receive TCP messages.
pub type Socket

type DoNotLeak

/// Errors that can occur when establishing a TCP connection.
///
pub type ConnectError {
  ConnectFailedIpv4(ipv4: Error)
  ConnectFailedIpv6(ipv6: Error)
  ConnectFailedBoth(ipv4: Error, ipv6: Error)
}

/// Errors that can occur when working with TCP sockets.
///
/// For more information on these errors see the Erlang documentation:
/// - https://www.erlang.org/doc/man/file#type-posix
/// - https://www.erlang.org/doc/man/inet#type-posix
///
pub type Error {
  // https://www.erlang.org/doc/man/inet#type-posix
  Closed
  Timeout
  Eaddrinuse
  Eaddrnotavail
  Eafnosupport
  Ealready
  Econnaborted
  Econnrefused
  Econnreset
  Edestaddrreq
  Ehostdown
  Ehostunreach
  Einprogress
  Eisconn
  Emsgsize
  Enetdown
  Enetunreach
  Enopkg
  Enoprotoopt
  Enotconn
  Enotty
  Enotsock
  Eproto
  Eprotonosupport
  Eprototype
  Esocktnosupport
  Etimedout
  Ewouldblock
  Exbadport
  Exbadseq
  Nxdomain
  // https://www.erlang.org/doc/man/file#type-posix
  Eacces
  Eagain
  Ebadf
  Ebadmsg
  Ebusy
  Edeadlk
  Edeadlock
  Edquot
  Eexist
  Efault
  Efbig
  Eftype
  Eintr
  Einval
  Eio
  Eisdir
  Eloop
  Emfile
  Emlink
  Emultihop
  Enametoolong
  Enfile
  Enobufs
  Enodev
  Enolck
  Enolink
  Enoent
  Enomem
  Enospc
  Enosr
  Enostr
  Enosys
  Enotblk
  Enotdir
  Enotsup
  Enxio
  Eopnotsupp
  Eoverflow
  Eperm
  Epipe
  Erange
  Erofs
  Espipe
  Esrch
  Estale
  Etxtbsy
  Exdev
}

pub type ConnectionOptions {
  ConnectionOptions(
    /// The hostname of the server to connect to.
    host: String,
    /// The port of the server to connect to.
    port: Int,
    /// A timeout in milliseconds for the connection to be established.
    ///
    /// If both IPv4 and IPv6 are being tried (the default) then this timeout
    /// will be used twice, so connecting in total could take twice this amount
    /// of time.
    ///
    /// Note that if the operating system returns a timeout then this package
    /// will also return a timeout, even if this timeout value has not been
    /// reached yet.
    ///
    /// The default is 1000ms.
    ///
    timeout: Int,
    /// What approach to take selecting between IPv4 and IPv6.
    ///
    /// The default is `Ipv6Preferred`.
    ///
    ip_version_preference: IpVersionPreference,
  )
}

/// Create a new set of connection options.
///
pub fn new(host: String, port port: Int) -> ConnectionOptions {
  ConnectionOptions(
    host: host,
    port: port,
    timeout: 1000,
    ip_version_preference: Ipv6Preferred,
  )
}

/// What approach to take selecting between IPv4 and IPv6.
/// See the `IpVersionPreference` type for documentation.
///
/// The default is `Ipv6Preferred`.
///
pub fn ip_version_preference(
  options: ConnectionOptions,
  preference: IpVersionPreference,
) -> ConnectionOptions {
  ConnectionOptions(..options, ip_version_preference: preference)
}

/// Specify a timeout for the connection to be established, in milliseconds.
///
/// The default is 1000ms.
///
pub fn timeout(
  options: ConnectionOptions,
  milliseconds timeout: Int,
) -> ConnectionOptions {
  ConnectionOptions(..options, timeout: timeout)
}

/// What approach to take with selection between IPv4 and IPv6
///
/// IPv6 is superior when available, but unfortunately not all networks
/// support it.
///
pub type IpVersionPreference {
  /// Only connect over IPv4.
  ///
  Ipv4Only
  /// Attempt to connect over IPv4 first, attempting IPv6 if connection with
  /// IPv4 did not succeed.
  ///
  Ipv4Preferred
  /// Only connect over IPv6.
  ///
  Ipv6Only
  /// Attempt to connect over IPv6 first, attempting IPv4 if connection with
  /// IPv6 did not succeed.
  ///
  Ipv6Preferred
}

/// Establish a TCP connection to the server specified in the connection
/// options.
///
/// Returns an error if the connection could not be established.
///
/// The socket is created in passive mode, meaning the the `receive` function is
/// to be called to receive packets from the client. The
/// `receive_next_packet_as_message` function can be used to switch the socket
/// to active mode and receive the next packet as an Erlang message.
///
pub fn connect(options: ConnectionOptions) -> Result(Socket, ConnectError) {
  let host = charlist.from_string(options.host)
  let connect = fn(inet) {
    let gen_options = [
      inet,
      // When data is received on the socket queue it in the TCP stack rather than
      // sending it as an Erlang message to the socket owner's inbox.
      Active(passive()),
      // We want the data from the socket as bit arrays please, not lists.
      Mode(Binary),
    ]
    gen_tcp_connect(host, options.port, gen_options, options.timeout)
  }
  case options.ip_version_preference {
    Ipv4Only -> connect(Inet) |> result.map_error(ConnectFailedIpv4)
    Ipv6Only -> connect(Inet6) |> result.map_error(ConnectFailedIpv6)

    Ipv4Preferred ->
      case connect(Inet) {
        Ok(conn) -> Ok(conn)
        Error(ipv4) ->
          case connect(Inet6) {
            Ok(conn) -> Ok(conn)
            Error(ipv6) -> Error(ConnectFailedBoth(ipv4:, ipv6:))
          }
      }

    Ipv6Preferred ->
      case connect(Inet6) {
        Ok(conn) -> Ok(conn)
        Error(ipv6) ->
          case connect(Inet) {
            Ok(conn) -> Ok(conn)
            Error(ipv4) -> Error(ConnectFailedBoth(ipv4:, ipv6:))
          }
      }
  }
}

type ModeValue {
  Binary
}

type ActiveValue

@external(erlang, "mug_ffi", "active")
fn active() -> ActiveValue

@external(erlang, "mug_ffi", "passive")
fn passive() -> ActiveValue

@external(erlang, "mug_ffi", "active_once")
fn active_once() -> ActiveValue

type GenTcpOption {
  /// Use IPv4
  Inet
  /// Use IPv6
  Inet6
  Active(ActiveValue)
  Mode(ModeValue)
}

@external(erlang, "gen_tcp", "connect")
fn gen_tcp_connect(
  host: Charlist,
  port: Int,
  options: List(GenTcpOption),
  timeout: Int,
) -> Result(Socket, Error)

/// Send a message to the client.
///
pub fn send(socket: Socket, message: BitArray) -> Result(Nil, Error) {
  send_builder(socket, bytes_tree.from_bit_array(message))
}

/// Send a message to the client, the data in `BytesBuilder`. Using this function
/// is more efficient turning an `BytesBuilder` or a `StringBuilder` into a
/// `BitArray` to use with the `send` function.
///
@external(erlang, "mug_ffi", "send")
pub fn send_builder(socket: Socket, packet: BytesTree) -> Result(Nil, Error)

/// Receive a message from the client.
///
/// Errors if the socket is closed, if the timeout is reached, or if there is
/// some other problem receiving the packet.
///
pub fn receive(
  socket: Socket,
  timeout_milliseconds timeout: Int,
) -> Result(BitArray, Error) {
  gen_tcp_receive(socket, 0, timeout_milliseconds: timeout)
}

/// Receive the specified number of bytes from the client, unless the socket
/// was closed, from the other side. In that case, the last read may return
/// less bytes.
/// If the specified number of bytes is not available to read from the socket
/// then the function will block until the bytes are available, or until the
/// timeout is reached.
/// This directly calls the underlying Erlang function `gen_tcp:recv/3`.
///
/// Errors if the socket is closed, if the timeout is reached, or if there is
/// some other problem receiving the packet.
pub fn receive_exact(
  socket: Socket,
  byte_size size: Int,
  timeout_milliseconds timeout: Int,
) -> Result(BitArray, Error) {
  gen_tcp_receive(socket, size, timeout_milliseconds: timeout)
}

@external(erlang, "gen_tcp", "recv")
fn gen_tcp_receive(
  socket: Socket,
  read_bytes_num: Int,
  timeout_milliseconds timeout: Int,
) -> Result(BitArray, Error)

/// Close the socket, ensuring that any data buffered in the socket is flushed
/// to the operating system kernel socket first.
///
@external(erlang, "mug_ffi", "shutdown")
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
  set_socket_options(socket, [Active(active_once())])
  Nil
}

/// Switch the socket to active mode, meaning that all packets received on
/// the socket will be sent as Erlang messages to the socket owner's inbox.
///
/// See `receive_next_packet_as_message` for details.
///
pub fn receive_all_packets_as_messages(socket: Socket) -> Nil {
  set_socket_options(socket, [Active(active())])
  Nil
}

/// Switch the socket to passive mode, meaning that packets received on the 
/// socket will no longer be sent as Erlang messages to the socket owner's inbox.
///
/// Cancels the effect of `receive_next_packet_as_message` or 
/// `receive_all_packets_as_messages`, if they have been called.
///
pub fn stop_receiving_packets_as_messages(socket: Socket) -> Nil {
  set_socket_options(socket, [Active(passive())])
  Nil
}

@external(erlang, "inet", "setopts")
fn set_socket_options(socket: Socket, options: List(GenTcpOption)) -> DoNotLeak

/// Messages that can be sent by the socket to the process that controls it.
///
pub type TcpMessage {
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
pub fn select_tcp_messages(
  selector: process.Selector(t),
  mapper: fn(TcpMessage) -> t,
) -> process.Selector(t) {
  let tcp = atom.create("tcp")
  let closed = atom.create("tcp_closed")
  let error = atom.create("tcp_error")

  selector
  |> process.select_record(tcp, 2, map_tcp_message(mapper))
  |> process.select_record(closed, 1, map_tcp_message(mapper))
  |> process.select_record(error, 2, map_tcp_message(mapper))
}

fn map_tcp_message(mapper: fn(TcpMessage) -> t) -> fn(Dynamic) -> t {
  fn(message) { mapper(unsafe_decode(message)) }
}

@external(erlang, "mug_ffi", "coerce_tcp_message")
fn unsafe_decode(message: Dynamic) -> TcpMessage
