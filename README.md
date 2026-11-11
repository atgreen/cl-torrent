# cl-torrent

`cl-torrent` is a reusable, transport-independent BitTorrent v2 library for
Common Lisp. It creates and validates canonical BEP 52 metainfo, verifies files
through Merkle piece layers, speaks the standard BitTorrent peer wire, and can
discover peers through HTTP or HTTPS trackers.

The peer-wire code consumes a bidirectional binary stream, so an application can
select interoperable TCP, `pure-tls`, or ag-gRPC without changing torrent or
storage logic.

## Systems

| ASDF system | Purpose | Additional dependencies |
| --- | --- | --- |
| `cl-torrent` | Bencode, BEP 52 metainfo, piece store, peer wire, transport protocol | `ironclad`, `trivial-utf-8` |
| `cl-torrent/tcp` | Standard TCP peer connections and listeners | `usocket`, `bordeaux-threads` |
| `cl-torrent/tls` | Composable TLS decorator for any peer transport | `pure-tls` |
| `cl-torrent/tracker-http` | BEP 3 HTTP(S), BEP 23 IPv4 and BEP 7 IPv6 tracker client | `usocket`, `pure-tls` |
| `cl-torrent/ag-grpc` | Binary stream bridge over ag-gRPC bidi messages | `ag-grpc`, `trivial-gray-streams` |
| `cl-torrent/tests` | Complete test suite | `fiveam` and all optional systems |

The core system has no socket, TLS, HTTP, or gRPC dependency. All TLS
functionality is implemented with `pure-tls`.

## Quick start

```lisp
(asdf:load-system "cl-torrent")

(let ((torrent (cl-torrent:make-torrent
                #P"artifact.bin"
                :announce "https://tracker.example/announce")))
  (with-open-file (output #P"artifact.torrent"
                          :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-sequence (cl-torrent:torrent-metainfo torrent) output)))
```

Run the included command-line example from the repository root:

```sh
sbcl --script examples/create-torrent.lisp artifact.bin artifact.torrent \
  https://tracker.example/announce
```

Install locked dependencies and run the suite with:

```sh
ocicl install
sbcl --noinform --non-interactive \
  --eval '(require :asdf)' \
  --eval '(asdf:load-asd (truename "cl-torrent.asd"))' \
  --eval '(asdf:test-system "cl-torrent")'
```

## Peer transports

`open-peer-stream`, `start-peer-listener`, and `stop-peer-listener` form the
transport protocol. `cl-torrent/tcp` is compatible with ordinary BitTorrent TCP
peers. `cl-torrent/tls` decorates another transport and accepts application-owned
certificate, key, trust context, mTLS, hostname, and ALPN configuration.

The ag-gRPC adapter maps each flushed peer-wire buffer to one bidi-stream
message. Applications supply protobuf encode/decode callbacks, which keeps
generated service types outside this library:

```lisp
(cl-torrent:make-ag-grpc-transport
 (lambda (endpoint)
   (declare (ignore endpoint))
   (ag-grpc:call-bidirectional-streaming
    channel "/example.Torrent/Peer"
    :response-type 'example:peer-chunk))
 :encode (lambda (octets) (make-instance 'example:peer-chunk :data octets))
 :decode (lambda (message) (slot-value message 'example::data)))
```

Server ownership also remains with the application. Optional listener callbacks
adapt an existing ag-gRPC server and generated bidi handler to the same
cl-torrent listener protocol.

## Tracker compatibility

`announce-tracker` sends BEP 3 parameters, truncates a v2 SHA-256 infohash to the
20 bytes required by BEP 52, and handles compact and dictionary peer responses.
HTTPS uses `pure-tls` with required certificate and hostname verification.
Callers may supply a `pure-tls` context for private trust roots.

## Scope

Version 0.1 targets canonical BitTorrent v2 metainfo and the standard TCP peer
wire. Hybrid/v1 torrent creation, UDP trackers, DHT, PEX, magnet links, uTP, and
NAT traversal are not implemented.

## Author

Anthony Green <green@moxielogic.com>

## License

MIT. See `LICENSE`.
