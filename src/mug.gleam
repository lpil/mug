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
  /// Connection closed
  Closed
  /// Operation timed out
  Timeout
  /// Address already in use
  Eaddrinuse
  /// Cannot assign requested address
  Eaddrnotavail
  /// Address family not supported
  Eafnosupport
  /// Operation already in progress
  Ealready
  /// Connection aborted
  Econnaborted
  /// Connection refused
  Econnrefused
  /// Connection reset by peer
  Econnreset
  /// Destination address required
  Edestaddrreq
  /// Host is down
  Ehostdown
  /// No route to host
  Ehostunreach
  /// Operation now in progress
  Einprogress
  /// Socket is already connected
  Eisconn
  /// Message too long
  Emsgsize
  /// Network is down
  Enetdown
  /// Network is unreachable
  Enetunreach
  /// Package not installed
  Enopkg
  /// Protocol not available
  Enoprotoopt
  /// Socket is not connected
  Enotconn
  /// Inappropriate ioctl for device
  Enotty
  /// Socket operation on non-socket
  Enotsock
  /// Protocol error
  Eproto
  /// Protocol not supported
  Eprotonosupport
  /// Protocol wrong type for socket
  Eprototype
  /// Socket type not supported
  Esocktnosupport
  /// Connection timed out
  Etimedout
  /// Operation would block
  Ewouldblock
  /// Bad port number
  Exbadport
  /// Bad sequence number
  Exbadseq
  /// Non-existent domain
  Nxdomain

  // https://www.erlang.org/doc/man/file#type-posix
  /// Permission denied
  Eacces
  /// Resource temporarily unavailable
  Eagain
  /// Bad file descriptor
  Ebadf
  /// Bad message
  Ebadmsg
  /// Device or resource busy
  Ebusy
  /// Resource deadlock avoided
  Edeadlk
  /// Resource deadlock avoided
  Edeadlock
  /// Disk quota exceeded
  Edquot
  /// File exists
  Eexist
  /// Bad address
  Efault
  /// File too large
  Efbig
  /// Inappropriate file type or format
  Eftype
  /// Interrupted system call
  Eintr
  /// Invalid argument
  Einval
  /// Input/output error
  Eio
  /// Is a directory
  Eisdir
  /// Too many levels of symbolic links
  Eloop
  /// Too many open files
  Emfile
  /// Too many links
  Emlink
  /// Multihop attempted
  Emultihop
  /// File name too long
  Enametoolong
  /// Too many open files in system
  Enfile
  /// No buffer space available
  Enobufs
  /// No such device
  Enodev
  /// No locks available
  Enolck
  /// Link has been severed
  Enolink
  /// No such file or directory
  Enoent
  /// Out of memory
  Enomem
  /// No space left on device
  Enospc
  /// Out of streams resources
  Enosr
  /// Device not a stream
  Enostr
  /// Function not implemented
  Enosys
  /// Block device required
  Enotblk
  /// Not a directory
  Enotdir
  /// Operation not supported
  Enotsup
  /// No such device or address
  Enxio
  /// Operation not supported on socket
  Eopnotsupp
  /// Value too large for defined data type
  Eoverflow
  /// Operation not permitted
  Eperm
  /// Broken pipe
  Epipe
  /// Result too large
  Erange
  /// Read-only file system
  Erofs
  /// Illegal seek
  Espipe
  /// No such process
  Esrch
  /// Stale file handle
  Estale
  /// Text file busy
  Etxtbsy
  /// Cross-device link
  Exdev
}

/// Convert an error into a human-readable description
///
pub fn describe_error(error: Error) -> String {
  case error {
    Closed -> "Connection closed"
    Timeout -> "Operation timed out"
    Eaddrinuse -> "Address already in use"
    Eaddrnotavail -> "Cannot assign requested address"
    Eafnosupport -> "Address family not supported"
    Ealready -> "Operation already in progress"
    Econnaborted -> "Connection aborted"
    Econnrefused -> "Connection refused"
    Econnreset -> "Connection reset by peer"
    Edestaddrreq -> "Destination address required"
    Ehostdown -> "Host is down"
    Ehostunreach -> "No route to host"
    Einprogress -> "Operation now in progress"
    Eisconn -> "Socket is already connected"
    Emsgsize -> "Message too long"
    Enetdown -> "Network is down"
    Enetunreach -> "Network is unreachable"
    Enopkg -> "Package not installed"
    Enoprotoopt -> "Protocol not available"
    Enotconn -> "Socket is not connected"
    Enotty -> "Inappropriate ioctl for device"
    Enotsock -> "Socket operation on non-socket"
    Eproto -> "Protocol error"
    Eprotonosupport -> "Protocol not supported"
    Eprototype -> "Protocol wrong type for socket"
    Esocktnosupport -> "Socket type not supported"
    Etimedout -> "Connection timed out"
    Ewouldblock -> "Operation would block"
    Exbadport -> "Bad port number"
    Exbadseq -> "Bad sequence number"
    Nxdomain -> "Non-existent domain"
    Eacces -> "Permission denied"
    Eagain -> "Resource temporarily unavailable"
    Ebadf -> "Bad file descriptor"
    Ebadmsg -> "Bad message"
    Ebusy -> "Device or resource busy"
    Edeadlk -> "Resource deadlock avoided"
    Edeadlock -> "Resource deadlock avoided"
    Edquot -> "Disk quota exceeded"
    Eexist -> "File exists"
    Efault -> "Bad address"
    Efbig -> "File too large"
    Eftype -> "Inappropriate file type or format"
    Eintr -> "Interrupted system call"
    Einval -> "Invalid argument"
    Eio -> "Input/output error"
    Eisdir -> "Is a directory"
    Eloop -> "Too many levels of symbolic links"
    Emfile -> "Too many open files"
    Emlink -> "Too many links"
    Emultihop -> "Multihop attempted"
    Enametoolong -> "File name too long"
    Enfile -> "Too many open files in system"
    Enobufs -> "No buffer space available"
    Enodev -> "No such device"
    Enolck -> "No locks available"
    Enolink -> "Link has been severed"
    Enoent -> "No such file or directory"
    Enomem -> "Out of memory"
    Enospc -> "No space left on device"
    Enosr -> "Out of streams resources"
    Enostr -> "Device not a stream"
    Enosys -> "Function not implemented"
    Enotblk -> "Block device required"
    Enotdir -> "Not a directory"
    Enotsup -> "Operation not supported"
    Enxio -> "No such device or address"
    Eopnotsupp -> "Operation not supported on socket"
    Eoverflow -> "Value too large for defined data type"
    Eperm -> "Operation not permitted"
    Epipe -> "Broken pipe"
    Erange -> "Result too large"
    Erofs -> "Read-only file system"
    Espipe -> "Illegal seek"
    Esrch -> "No such process"
    Estale -> "Stale file handle"
    Etxtbsy -> "Text file busy"
    Exdev -> "Cross-device link"
  }
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
