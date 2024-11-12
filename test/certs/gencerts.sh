#!/bin/bash
set -eu

if [ ! -f gleam.toml ]; then
  echo "Please run this script in the project root" >&2
fi

pushd test/certs
if [[ -f server.crt || -f server.key || -f server.csr || -f ca.crt || -f ca.key ]]; then
  popd
  echo "A key already exists in test/keys. Proceeding will overwrite it!"
  echo "Press ENTER to proceed, CTRL+C to cancel."
  read
  pushd test/certs
fi

echo "Removing exiting certificates..."
find . -maxdepth 1 -type f ! -name 'gencerts.sh' -delete

# echo "Generating the certificate..."

# yes '' | openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -sha256 -days 3650 -nodes

echo "Generating the CA certificate and key..."
yes '' | openssl req -x509 -new -nodes -keyout ca.key -out ca.crt -sha256 -days 3650
echo "Generating the certificate key and request..."
yes '' | openssl req -newkey rsa:4096 -keyout server.key -out server.csr -nodes
echo "Generating the certificate..."
openssl x509 -req -in server.csr -outform pem -CA ca.crt -CAkey ca.key -CAcreateserial -out server.crt -days 3650 -sha256

popd
