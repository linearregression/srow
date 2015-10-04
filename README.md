Srow
====

An OTP application that enables asynchronous RPC - a failure on single
byte or message transfer never blocks, or affects others unlike RPC on
TCP. It utilizes UDP as transport but it guarantees soft-realtime
message transfer and will have compressed/encrypted mode. It does not
guarantee message reachability in the sense of byte stream, but in the
context of each message. Each message never affect or block each other.

This is an experimental transport library that will replaced by BERT
on QUIC, or a yet another simpler QUIC.

Build
-----

    $ rebar3 compile

Supervision Tree
----------------

```
sup -+- listener
     +- gateway_sup -+- gateway to node A
                     +- gateway to node B
```
