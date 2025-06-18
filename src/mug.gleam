import gleam/bytes_tree.{type BytesTree}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import mug/internal/ssl_options.{
  type SslOptionName, Cacertfile, Cacerts, CertsKeys, Verify,
}
import mug/internal/system_cacerts

pub type TcpSocket

pub type SslSocket

pub opaque type Socket {
  TcpSocket(TcpSocket)
  SslSocket(SslSocket)
}

/// Returns True if the given socket is a TLS connection (started either
/// with `connect` or `upgrade`). Returns False if the socket is a
/// plain-text TCP connection.
///
pub fn socket_is_ssl(socket: Socket) {
  case socket {
    SslSocket(_) -> True
    TcpSocket(_) -> False
  }
}

type DoNotLeak

/// Error occured during a TLS operation.
///
/// Reference: https://www.erlang.org/doc/apps/ssl/ssl.html#t:tls_alert/0
pub type TlsAlert {
  CloseNotify
  UnexpectedMessage
  BadRecordMac
  RecordOverflow
  HandshakeFailure
  BadCertificate
  UnsupportedCertificate
  CertificateRevoked
  CertificateExpired
  CertificateUnknown
  IllegalParameter
  UnknownCa
  AccessDenied
  DecodeError
  DecryptError
  ExportRestriction
  ProtocolVersion
  InsufficientSecurity
  InternalError
  InappropriateFallback
  UserCanceled
  NoRenegotiation
  UnsupportedExtension
  CertificateUnobtainable
  UnrecognizedName
  BadCertificateStatusResponse
  BadCertificateHashValue
  UnknownPskIdentity
  NoApplicationProtocol
}

/// Errors that can occur when working with TCP sockets.
///
/// For more information on these errors see the Erlang documentation:
/// - https://www.erlang.org/doc/man/file#type-posix
/// - https://www.erlang.org/doc/man/inet#type-posix
///
pub type Error {
  /// Unable to get the OS supplied CA certificates. This error is only returned if the
  /// `use_system_cacerts` option is set to `true` and the system's CA certificates
  /// could not be retrieved.
  SystemCacertificatesGetError(system_cacerts.SystemCacertificatesGetError)

  // For connect only
  /// An invalid option was passed
  Options(opt: Dynamic)

  /// TLS connection failed
  TlsAlert(alert: TlsAlert, description: String)

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
    /// TLS options
    tls_opts: TlsConnectionOptions,
  )
}

pub type TlsConnectionOptions {
  /// Do not use TLS, use plain TCP
  NoTls
  /// Start with a TLS connection
  UseTls(TlsOptions)
}

/// Configuration for TLS connections.
pub type TlsOptions {
  TlsOptions(verification_method: TlsVerificationMethod)
}

/// The certificates to use 
pub type TlsVerificationMethod {
  /// Uses these CA certificates and regular certificates to verify the server's certificate.
  ///
  /// The `use_system_cacerts` option makes mug use the system's CA certificates, which are 
  /// usually set to a group of competent CAs who sign most of the web's certificates.  
  /// You may specifiy your own CA certificates with the `cacerts` option, and custom
  /// certificates and their keys with the `certificates_keys` option.
  ///
  /// Note that specifying a PEM encoded CA certificate file will result in the system CA
  /// certificates not being used. This is because of how the underlying [erlang implementation](https://www.erlang.org/doc/apps/ssl/ssl.html#t:client_option/0)
  /// works. System CA certificates are passed as DER-encoded certificates, and DER encoded
  /// certs override PEM-encoded ones. Therefore, if you wish to use a PEM encoded CA cert along
  /// with the system's CA, you should decode the PEM into a DER using (`public_key:pem_decode`)[https://www.erlang.org/doc/apps/public_key/public_key.html#pem-api]
  /// and use the DerEncodedCaCertificates variant instead.
  Certificates(
    use_system_cacerts: Bool,
    cacerts: Option(CaCertificates),
    certificates_keys: List(CertificatesKeys),
  )
  /// Do not verify certificates. While this does allow you to use self-signed certificates.
  /// It is highly recommended to not skip verification, add a custom CA instead.
  DangerouslyDisableVerification
}

/// The CA certificates to use
pub type CaCertificates {
  /// Use these der-encoded certificates as CA certificates.
  DerEncodedCaCertificates(ca_certificates: List(BitArray))
  /// Path to a pem-encoded file which contains CA certificates.
  /// If this option is specified, then the system CA will not be used.
  PemEncodedCaCertificates(ca_certificates_file: String)
}

pub type CertificatesKeys {
  /// A list of DER-encoded certificates and their corresponding key.
  DerEncodedCertificatesKeys(certificate: List(BitArray), key: DerEncodedKey)
  /// Path to a file containing PEM-encoded certificates and their key, with an optional
  /// password associated with the file containing the key.
  PemEncodedCertificatesKeys(
    certificate_path: String,
    key_path: String,
    password: Option(BitArray),
  )
}

pub type DerEncodedKeyAlgorithm {
  RSAPrivateKey
  DSAPrivateKey
  ECPrivateKey
  PrivateKeyInfo
}

pub type DerEncodedKey {
  /// A der-encoded key.  
  DerEncodedKey(algorithm: DerEncodedKeyAlgorithm, key: BitArray)
}

/// Create a new set of connection options.
///
pub fn new(host: String, port port: Int) -> ConnectionOptions {
  ConnectionOptions(host: host, port: port, timeout: 1000, tls_opts: NoTls)
}

/// Specify a timeout for the connection to be established.
///
pub fn timeout(
  options: ConnectionOptions,
  milliseconds timeout: Int,
) -> ConnectionOptions {
  ConnectionOptions(..options, timeout: timeout)
}

/// Use TLS for the connection.
///
/// This function must be called before any other functions that modify the TLS
/// connection options.
///
/// Uses the system's CA certificates to verify the server's certificate.
///
pub fn with_tls(options) {
  ConnectionOptions(
    ..options,
    tls_opts: UseTls(
      TlsOptions(
        verification_method: Certificates(
          use_system_cacerts: True,
          cacerts: None,
          certificates_keys: [],
        ),
      ),
    ),
  )
}

/// Do not verify the server's certificate. This is dangerous and is not
/// recommended. Use a custom certificate instead.
pub fn dangerously_disable_verification(options) {
  ConnectionOptions(
    ..options,
    tls_opts: UseTls(TlsOptions(
      verification_method: DangerouslyDisableVerification,
    )),
  )
}

/// Do not use the system's CA certificates to verify the server's certificate.
///
/// This is useful when you want to use your own CA certificates to verify the
/// server's certificate. If verification is disabled, this function does nothing.
pub fn no_system_cacerts(options) {
  ConnectionOptions(..options, tls_opts: case options.tls_opts {
    UseTls(TlsOptions(verification_method)) ->
      UseTls(
        TlsOptions(verification_method: case verification_method {
          Certificates(_, cacerts, certificates_keys) ->
            Certificates(False, cacerts, certificates_keys)
          _ -> verification_method
        }),
      )
    _ -> options.tls_opts
  })
}

/// Set the following CA Certificates for the connection. These CA certificates will be used to check
/// the TLS server's certificate. If a PEM-encoded CA certfile is provided, the system's CA will not
/// be used.
///
/// If verification is disabled, this function does nothing.
///
pub fn cacerts(
  options: ConnectionOptions,
  cacerts: CaCertificates,
) -> ConnectionOptions {
  ConnectionOptions(..options, tls_opts: case options.tls_opts {
    UseTls(TlsOptions(verification_method)) ->
      UseTls(
        TlsOptions(verification_method: case verification_method {
          Certificates(system, _, certificates_keys) ->
            Certificates(system, Some(cacerts), certificates_keys)
          _ -> verification_method
        }),
      )
    _ -> options.tls_opts
  })
}

/// Set the certs_keys TLS [common cert option](https://www.erlang.org/doc/apps/ssl/ssl.html#t:common_option_cert/0).  
///
/// The certificates_keys can be specified in two ways, a list of der-encoded certificates with their corresponding key, or
/// the paths to a certfile and keyfile containing one or more PEM-certificates and their corresponding key. A password
/// may also be specified for the file containing the key. Note that the entity certificate must be the first certificate
/// in the der-encoded list or the pem-encoded file.
///
/// For maximum interoperability, the certificates in the chain should be in the correct order, as the chain will be 
/// sent as-is to the peer. If chain certificates are not provided, certificates from the configured trusted CA certificates 
/// will be used to construct the chain.
///
/// If verification is disabled, this function does nothing.
///
pub fn certificates_keys(
  options: ConnectionOptions,
  certificates_keys certificates_keys: List(CertificatesKeys),
) -> ConnectionOptions {
  ConnectionOptions(..options, tls_opts: case options.tls_opts {
    UseTls(TlsOptions(verification_method)) ->
      UseTls(
        TlsOptions(verification_method: case verification_method {
          Certificates(system, cacerts, _) ->
            Certificates(system, cacerts, certificates_keys)
          _ -> verification_method
        }),
      )
    _ -> options.tls_opts
  })
}

type GenTcpOptionName {
  Active
  Mode
}

type ModeValue {
  Binary
}

type ActiveValue {
  Once
}

type GenTcpOption =
  #(GenTcpOptionName, Dynamic)

@external(erlang, "gen_tcp", "connect")
fn gen_tcp_connect(
  host: Charlist,
  port: Int,
  options: List(GenTcpOption),
  timeout: Int,
) -> Result(TcpSocket, Error)

@external(erlang, "mug_ffi", "ssl_connect")
fn ssl_connect(
  host: Charlist,
  port: Int,
  options: List(SslOption),
  timeout: Int,
) -> Result(SslSocket, Error)

type VerifyValue {
  VerifyPeer
  VerifyNone
}

type SslOption =
  #(SslOptionName, Dynamic)

fn get_tls_options(vm: TlsVerificationMethod) -> Result(List(SslOption), Error) {
  let opts = [
    // When data is received on the socket queue it in the TCP stack rather than
    // sending it as an Erlang message to the socket owner's inbox.
    #(ssl_options.Active, dynamic.from(False)),
    // We want the data from the socket as bit arrays please, not lists.
    #(ssl_options.Mode, dynamic.from(Binary)),
  ]
  case vm {
    DangerouslyDisableVerification ->
      Ok([#(Verify, dynamic.from(VerifyNone)), ..opts])
    Certificates(system, cacerts, certificates_keys) -> {
      use cacerts <- result.try(get_cacerts_opt(system, cacerts))
      Ok([
        #(Verify, dynamic.from(VerifyPeer)),
        cacerts,
        #(CertsKeys, dynamic.from(get_certs_keys(certificates_keys))),
      ])
    }
  }
}

fn get_cacerts_opt(
  system: Bool,
  cacerts: Option(CaCertificates),
) -> Result(SslOption, Error) {
  case system, cacerts {
    False, Some(DerEncodedCaCertificates(cacerts)) ->
      Ok(#(Cacerts, dynamic.from(cacerts)))
    True, Some(DerEncodedCaCertificates(cacerts)) -> {
      let certs =
        system_cacerts.get() |> result.map_error(SystemCacertificatesGetError)
      use certs <- result.try(certs)
      Ok(#(Cacerts, dynamic.from(list.flatten([certs.0, cacerts]))))
    }
    _, Some(PemEncodedCaCertificates(cacerts)) ->
      Ok(#(Cacertfile, dynamic.from(string.to_utf_codepoints(cacerts))))
    True, None -> {
      let certs =
        system_cacerts.get() |> result.map_error(SystemCacertificatesGetError)
      use certs <- result.try(certs)
      Ok(#(Cacerts, dynamic.from(certs)))
    }
    False, None -> Ok(#(Cacerts, dynamic.from([])))
  }
}

@external(erlang, "mug_ffi", "get_certs_keys")
fn get_certs_keys(certs_keys: List(CertificatesKeys)) -> certs_keys

/// Establish a TCP/TLS connection to the server specified in the connection
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
  let host = charlist.from_string(options.host)
  case options.tls_opts {
    UseTls(TlsOptions(vm)) -> {
      use opts <- result.try(get_tls_options(vm))
      ssl_connect(host, options.port, opts, options.timeout)
      |> result.map(SslSocket)
    }
    _ -> {
      let gen_options = [
        // When data is received on the socket queue it in the TCP stack rather than
        // sending it as an Erlang message to the socket owner's inbox.
        #(Active, dynamic.from(False)),
        // We want the data from the socket as bit arrays please, not lists.
        #(Mode, dynamic.from(Binary)),
      ]
      gen_tcp_connect(host, options.port, gen_options, options.timeout)
      |> result.map(TcpSocket)
    }
  }
}

@external(erlang, "mug_ffi", "ssl_upgrade")
fn ssl_upgrade(
  socket: TcpSocket,
  options: List(SslOption),
  timeout: Int,
) -> Result(SslSocket, Error)

/// Upgrade a plain TCP connection to TLS.
///
/// Accepts a socket and performs the client-side TLS handshake. This
/// may not work on all TCP servers.
///
/// If the socket is already a TLS socket, this function does nothing.
///
/// Returns an error if the connection could not be established.
///
/// The socket is created in passive mode, meaning the the `receive` function is
/// to be called to receive packets from the client. The
/// `receive_next_packet_as_message` function can be used to switch the socket
/// to active mode and receive the next packet as an Erlang message.
///
pub fn upgrade(
  socket: Socket,
  verification_method vm: TlsVerificationMethod,
  timeout timeout: Int,
) -> Result(Socket, Error) {
  case socket {
    TcpSocket(socket) -> {
      use opts <- result.try(get_tls_options(vm))
      ssl_upgrade(socket, opts, timeout)
      |> result.map(SslSocket)
    }
    socket -> Ok(socket)
  }
}

/// Attempts to downgrade the connection. If downgrade is not successful, the socket is closed.
///
/// On successful downgrade, it returns the downgraded TCP socket, and *optionally* some binary data
/// that must be treated as the first bytes received on the downgraded connection.  
/// If the connection gets closed instead of getting downgraded, then the `Closed` error is returned.  
///
/// If this function is called on a TcpSocket, it will return a timeout error.
///
/// Internally, it uses [`ssl:close/2`](https://www.erlang.org/doc/apps/ssl/ssl.html#close/2) 
/// to perform the downgrade, which returns `ok` if the socket is closed, while this function returns
/// an error. It is recommended against using this function to close the socket. Use `shutdown` instead.
///
@external(erlang, "mug_ffi", "ssl_downgrade")
pub fn downgrade(
  socket: Socket,
  milliseconds timeout: Int,
) -> Result(#(TcpSocket, Option(BitArray)), Error)

/// Send a packet to the client.
///
pub fn send(socket: Socket, packet: BitArray) -> Result(Nil, Error) {
  send_builder(socket, bytes_tree.from_bit_array(packet))
}

/// Send a packet to the client, the data in `BytesBuilder`. Using this function
/// is more efficient than turning a `BytesBuilder` or a `StringBuilder` into a
/// `BitArray` to use with the `send` function.
///
@external(erlang, "mug_ffi", "send")
pub fn send_builder(socket: Socket, packet: BytesTree) -> Result(Nil, Error)

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

@external(erlang, "mug_ffi", "recv")
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
  case socket {
    TcpSocket(socket) ->
      set_tcp_socket_options(socket, [#(Active, dynamic.from(Once))])
    SslSocket(socket) ->
      set_ssl_socket_options(socket, [#(ssl_options.Active, dynamic.from(Once))])
  }
  Nil
}

@external(erlang, "inet", "setopts")
fn set_tcp_socket_options(
  socket: TcpSocket,
  options: List(GenTcpOption),
) -> DoNotLeak

@external(erlang, "ssl", "setopts")
fn set_ssl_socket_options(
  socket: SslSocket,
  options: List(SslOption),
) -> DoNotLeak

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
  |> process.selecting_record3(tcp, unsafe_coerce_tcp_packet(mapper))
  |> process.selecting_record2(closed, unsafe_coerce_tcp_closed(mapper))
  |> process.selecting_record3(error, unsafe_coerce_to_tcp_error(mapper))
}

/// Configure a selector to receive messages from TLS sockets.
///
/// Note this will receive messages from all TLS sockets that the process
/// controls, rather than any specific one. If you wish to only handle messages
/// from one socket then use one process per socket.
///
pub fn selecting_tls_messages(
  selector: process.Selector(t),
  mapper: fn(TcpMessage) -> t,
) -> process.Selector(t) {
  let ssl = atom.create_from_string("ssl")
  let closed = atom.create_from_string("ssl_closed")
  let error = atom.create_from_string("ssl_error")

  selector
  |> process.selecting_record3(ssl, unsafe_coerce_tls_packet(mapper))
  |> process.selecting_record2(closed, unsafe_coerce_tls_closed(mapper))
  |> process.selecting_record3(error, unsafe_coerce_to_tls_tcp_error(mapper))
}

fn unsafe_coerce_tcp_packet(
  mapper: fn(TcpMessage) -> t,
) -> fn(Dynamic, Dynamic) -> t {
  fn(socket, data) {
    Packet(TcpSocket(unsafe_coerce(socket)), unsafe_coerce(data))
    |> mapper
  }
}

fn unsafe_coerce_tls_packet(
  mapper: fn(TcpMessage) -> t,
) -> fn(Dynamic, Dynamic) -> t {
  fn(socket, data) {
    Packet(SslSocket(unsafe_coerce(socket)), unsafe_coerce(data))
    |> mapper
  }
}

fn unsafe_coerce_tcp_closed(mapper: fn(TcpMessage) -> t) -> fn(Dynamic) -> t {
  fn(socket) {
    SocketClosed(TcpSocket(unsafe_coerce(socket)))
    |> mapper
  }
}

fn unsafe_coerce_tls_closed(mapper: fn(TcpMessage) -> t) -> fn(Dynamic) -> t {
  fn(socket) {
    SocketClosed(SslSocket(unsafe_coerce(socket)))
    |> mapper
  }
}

fn unsafe_coerce_to_tcp_error(
  mapper: fn(TcpMessage) -> t,
) -> fn(Dynamic, Dynamic) -> t {
  fn(socket, reason) {
    mapper(TcpError(TcpSocket(unsafe_coerce(socket)), unsafe_coerce(reason)))
  }
}

fn unsafe_coerce_to_tls_tcp_error(
  mapper: fn(TcpMessage) -> t,
) -> fn(Dynamic, Dynamic) -> t {
  fn(socket, reason) {
    mapper(TcpError(SslSocket(unsafe_coerce(socket)), unsafe_coerce(reason)))
  }
}

@external(erlang, "mug_ffi", "coerce")
fn unsafe_coerce(data: Dynamic) -> a
