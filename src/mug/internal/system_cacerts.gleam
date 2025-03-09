import gleam/dynamic.{type Dynamic}

/// Adapted from https://www.erlang.org/doc/apps/public_key/public_key#t:combined_cert/0
pub type CombinedCert =
  #(List(BitArray), #(Dynamic, Dynamic, Dynamic))

pub type SystemCacertificatesGetError {
  /// Error accessing CA certificate files
  Enoent
  /// No CA Certificate files found
  NoCacertsFound
  /// OS is not supported
  Enotsup
  /// Operation failed
  Eopnotsup
}

@external(erlang, "mug_ffi", "get_system_cacerts")
pub fn get() -> Result(CombinedCert, SystemCacertificatesGetError)
