# Cohttp-eio Examples

This directory contains examples illustrating different modes of use of the
cohttp-eio package.

## [`client_proxy.ml`](./client_proxy.ml)

This executable shows an example of how to set up proxying for client requests.

## Prerequisites

The following usage examples assumes

- you are working in root directory of this project,
- you have installed [mitmproxy](https://github.com/mitmproxy/mitmproxy),
- and that you have launched `mitmdump` on port `8888` in a separate terminal, with

  ``` sh
  mitmdum -p 8888
  ```

NOTE: We use mitmproxy because it allows us to test the https connection to the
proxy locally. However, it also requires using its own cacert for these use
cases. This example executable can also be exercised with
[tinyproxy](https://github.com/tinyproxy/tinyproxy), excluding https connections
to the proxy.

### Direct proxy for http requests

``` sh
dune exec cohttp-eio/examples/client_proxy.exe -- \
  --all-proxy=http://127.0.0.1:8888 \
  http://httpbin.io/hostname
```


### Tunnelling proxy for https requests to the remote host

``` sh
dune exec cohttp-eio/examples/client_proxy.exe -- \
  --cacert=$HOME/.mitmproxy/mitmproxy-ca-cert.pem \
  --all-proxy=http://127.0.0.1:8888 \
  https://httpbin.io/hostname
```

### Using an https connection to the proxy

This exercises our support for TLS over TLS.

``` sh
dune exec cohttp-eio/examples/client_proxy.exe -- \
  --cacert=$HOME/.mitmproxy/mitmproxy-ca-cert.pem \
  --all-proxy=https://127.0.0.1:8888 \
  https://httpbin.io/hostname
```
