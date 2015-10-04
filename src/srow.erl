-module(srow).

-export([send/2, self/0]).

-export([remote_host/1, remote_pid/1, remote_process/3]).

-record(srow_local_process,
        {pid = erlang:self() :: pid()}).

-record(srow_remote_process,
        {host :: inet:hostname(),
         port :: inet:ip_address(),
         pid :: pid()}).

send(#srow_local_process{pid=Pid}, Msg) ->
    Pid ! Msg;
send(#srow_remote_process{} = Remote, Msg) ->
    %% Just send this Massage with new #srow_local_process{}
    srow_gateway:send(Remote, Msg).

self() ->
    #srow_local_process{}.

remote_host(#srow_remote_process{host=H, port=P}) ->
    {H, P}.

remote_pid(#srow_remote_process{pid=Pid}) ->
    Pid.

remote_process(Host, Port, Pid) ->
    #srow_remote_process{host=Host, port=Port, pid=Pid}.
