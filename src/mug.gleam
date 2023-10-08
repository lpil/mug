import gleam/bit_builder.{type BitBuilder}
import gleam/erlang/charlist.{type Charlist}

pub type Socket

pub type Error {
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
}

// TODO: document
pub type ConnectionOptions {
  ConnectionOptions(host: String, port: Int, timeout: Int)
}

// TODO: document
pub fn new(host: String, port port: Int) -> ConnectionOptions {
  ConnectionOptions(host: host, port: port, timeout: 1000)
}

// TODO: document
pub fn timeout(
  options: ConnectionOptions,
  milliseconds timeout: Int,
) -> ConnectionOptions {
  ConnectionOptions(..options, timeout: timeout)
}

// TODO: document
pub fn connect(options: ConnectionOptions) -> Result(Socket, Error) {
  let gen_options = [
    // When data is received on the socket queue it in the TCP stack rather than
    // sending it as an Erlang message to the socket owner's inbox.
    Active(False),
    // We want the data from the socket as bit arrays please, not lists.
    Binary,
  ]
  let host = charlist.from_string(options.host)
  gen_tcp_connect(host, options.port, gen_options, options.timeout)
}

type GenTcpOption {
  Active(Bool)
  Binary
}

@external(erlang, "gen_tcp", "connect")
fn gen_tcp_connect(
  host: Charlist,
  port: Int,
  options: List(GenTcpOption),
  timeout: Int,
) -> Result(Socket, Error)

// TODO: document
pub fn send(socket: Socket, packet: BitArray) -> Result(Nil, Error) {
  send_builder(socket, bit_builder.from_bit_string(packet))
}

// TODO: document
@external(erlang, "mug_ffi", "send")
pub fn send_builder(socket: Socket, packet: BitBuilder) -> Result(Nil, Error)

// TODO: document
pub fn receive(
  socket: Socket,
  timeout_milliseconds timeout: Int,
) -> Result(BitArray, Error) {
  gen_tcp_receive(socket, 0, timeout_milliseconds: timeout)
}

@external(erlang, "gen_tcp", "recv")
fn gen_tcp_receive(
  socket: Socket,
  read_bytes_num: Int,
  timeout_milliseconds timeout: Int,
) -> Result(BitArray, Error)

// TODO: document
@external(erlang, "mug_ffi", "shutdown")
pub fn shutdown(socket: Socket) -> Result(Nil, Error)
