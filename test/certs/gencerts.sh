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
openssl genrsa -out ca.key 4096
openssl req -x509 -new -nodes -key ca.key -out ca.crt -days 3650 -subj '/C=XX/ST=XX/L=XX/O=XX/OU=XX/CN=localhost'
echo "Generating the certificate key..."
openssl genrsa -out server.key 4096
echo "Generating the certificate request..."
openssl req -new -key server.key -out server.csr -addext "subjectAltName=DNS:localhost" -subj '/C=XX/ST=XX/L=XX/O=XX/OU=XX/CN=localhost'
echo "Generating the certificate..."
openssl x509 -req -days 365 -in server.csr -outform pem -CA ca.crt -CAkey ca.key -CAcreateserial -out server.crt -days 3650 -copy_extensions copy

popd
