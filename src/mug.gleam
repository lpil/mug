import gleam/bytes_builder.{type BytesBuilder}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process

pub type Socket

type DoNotLeak

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
    /// Note that if the operating system returns a timeout then this package
    /// will also return a timeout, even if this timeout value has not been
    /// reached yet.
    timeout: Int,
    /// The IP version to use for the connection.
    ip_version: IpVersion,
  )
}

pub type IpVersion {
  Ipv4
  Ipv6
  /// Uses Happy Eyeballs (RFC 8305) to attempt connections on both IPv4 and IPv6
  Transparent
}

/// Create a new set of connection options.
///
pub fn new(host: String, port port: Int) -> ConnectionOptions {
  ConnectionOptions(
    host: host,
    port: port,
    timeout: 1000,
    ip_version: Transparent,
  )
}

/// Specify a timeout for the connection to be established.
///
pub fn timeout(
  options: ConnectionOptions,
  milliseconds timeout: Int,
) -> ConnectionOptions {
  ConnectionOptions(..options, timeout: timeout)
}

/// Specify the IP version to use for the connection.
///
pub fn ip_version(
  options: ConnectionOptions,
  ip_version: IpVersion,
) -> ConnectionOptions {
  ConnectionOptions(..options, ip_version: ip_version)
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
pub fn connect(options: ConnectionOptions) -> Result(Socket, Error) {
  case options.ip_version {
    Transparent -> connect_happy_eyballs(options)
    _ -> connect_single_stack(options)
  }
}

fn connect_happy_eyballs(options: ConnectionOptions) -> Result(Socket, Error) {
  let subject = process.new_subject()
  let pid = process.self()

  process.start(
    fn() {
      // 250ms is the suggested delay
      process.sleep(250)
      happy_eyeballs_attempt(
        subject,
        pid,
        ConnectionOptions(..options, ip_version: Ipv4),
      )
    },
    True,
  )
  process.start(
    fn() {
      happy_eyeballs_attempt(
        subject,
        pid,
        ConnectionOptions(..options, ip_version: Ipv6),
      )
    },
    True,
  )

  process.new_selector()
  |> process.selecting(subject, fn(res) { res })
  |> happy_eyeballs_receive(2)
}

fn happy_eyeballs_attempt(
  subject: process.Subject(Result(Socket, Error)),
  pid: process.Pid,
  options: ConnectionOptions,
) {
  let res = case connect_single_stack(options) {
    Ok(socket) -> {
      // could use process.subject_owner, however there's not really a good
      // reason to grab the pid multiple times if it can be passed
      let assert Ok(Nil) = controlling_process(socket, pid)
      Ok(socket)
    }
    Error(err) -> Error(err)
  }

  process.send(subject, res)
}

fn happy_eyeballs_receive(
  selector: process.Selector(Result(Socket, Error)),
  attempts: Int,
) {
  case process.select_forever(selector) {
    Ok(socket) -> Ok(socket)
    Error(err) ->
      case attempts {
        1 -> Error(err)
        _ -> happy_eyeballs_receive(selector, attempts - 1)
      }
  }
}

fn connect_single_stack(options: ConnectionOptions) -> Result(Socket, Error) {
  let gen_options = [
    Active(False),
    Mode(Binary),
    case options.ip_version {
      Ipv4 -> Inet
      Ipv6 -> Inet6
      _ -> panic as "unreachable"
    },
  ]
  let host = charlist.from_string(options.host)
  gen_tcp_connect(host, options.port, gen_options, options.timeout)
}

type GenTcpOption {
  Active(ActiveValue)
  Mode(ModeValue)
  Inet
  Inet6
}

type ActiveValue {
  // True
  False
  Once
}

type ModeValue {
  // List
  Binary
}

@external(erlang, "gen_tcp", "connect")
fn gen_tcp_connect(
  host: Charlist,
  port: Int,
  options: List(GenTcpOption),
  timeout: Int,
) -> Result(Socket, Error)

@external(erlang, "mug_ffi", "controlling_process")
fn controlling_process(socket: Socket, pid: process.Pid) -> Result(Nil, Error)

/// Send a packet to the client.
///
pub fn send(socket: Socket, packet: BitArray) -> Result(Nil, Error) {
  send_builder(socket, bytes_builder.from_bit_array(packet))
}

/// Send a packet to the client, the data in `BytesBuilder`. Using this function
/// is more efficient turning an `BytesBuilder` or a `StringBuilder` into a
/// `BitArray` to use with the `send` function.
///
@external(erlang, "mug_ffi", "send")
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

/// Close the socket, ensuring that any data buffered in the socket is flushed to the operating system kernel socket first.
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
  set_socket_options(socket, [Active(Once)])
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
pub fn selecting_tcp_messages(
  selector: process.Selector(t),
  mapper: fn(TcpMessage) -> t,
) -> process.Selector(t) {
  let tcp = atom.create_from_string("tcp")
  let closed = atom.create_from_string("tcp_closed")
  let error = atom.create_from_string("tcp_error")

  selector
  |> process.selecting_record3(tcp, unsafe_coerce_packet(mapper))
  |> process.selecting_record2(closed, unsafe_coerce_closed(mapper))
  |> process.selecting_record3(error, unsafe_coerce_to_tcp_error(mapper))
}

fn unsafe_coerce_packet(
  mapper: fn(TcpMessage) -> t,
) -> fn(Dynamic, Dynamic) -> t {
  fn(socket, data) {
    Packet(unsafe_coerce_to_socket(socket), dynamic.unsafe_coerce(data))
    |> mapper
  }
}

fn unsafe_coerce_closed(mapper: fn(TcpMessage) -> t) -> fn(Dynamic) -> t {
  fn(socket) {
    SocketClosed(unsafe_coerce_to_socket(socket))
    |> mapper
  }
}

fn unsafe_coerce_to_tcp_error(
  mapper: fn(TcpMessage) -> t,
) -> fn(Dynamic, Dynamic) -> t {
  fn(socket, reason) {
    mapper(TcpError(
      unsafe_coerce_to_socket(socket),
      dynamic.unsafe_coerce(reason),
    ))
  }
}

fn unsafe_coerce_to_socket(socket: Dynamic) -> Socket {
  dynamic.unsafe_coerce(socket)
}
