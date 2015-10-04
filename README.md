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

API example
-----------

```erlang
{ok, Pid} = srow_gateway:start_link({127,0,0,1}, 9799),
Remote = srow:remote_process({10,0,0,1}, 9799, your_registered_process),
ok = srow_gateway:send(Pid, Remote, {your, message, no, 1}),
```

```erlang
erlang:register(your_registered_process, self()),
{ok, Pid} = srow_gateway:start_link({10,0,0,1}, 9799),
receive Msg -> Msg end,
```
