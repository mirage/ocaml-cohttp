#!/bin/sh

# generate key
openssl genrsa -des3 -out server.key 1024

# strip password
mv server.key server.key.pass
openssl rsa -in server.key.pass -out server.key

# generate csr
openssl req -new -key server.key -out server.csr

# generate self singe certificate with csr
openssl x509 -req -days 3650 -in server.csr -signkey server.key -out server.crt
rm server.csr server.key.pass
