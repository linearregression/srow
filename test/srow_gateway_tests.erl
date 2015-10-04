-module(srow_gateway_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

start_link_one_msg_test() ->
    {ok, Pid1} = srow_gateway:start_link_2({127,0,0,1}, 10001),
    {ok, Pid2} = srow_gateway:start_link_2({127,0,0,1}, 10002),
    Self = srow:self(),
    %% sending to Pid2 from Pid1
    Remote = srow:remote_process({127,0,0,1}, 10002, erlang:self()),
    ok = srow_gateway:send(Pid1, Remote, {Self, foobar}),
    receive Msg ->
            %% ?debugVal(Msg),
            ?assertMatch({{srow_local_process, _}, foobar}, Msg)
    after 1024 ->
            ?assert(timeout)
    end,

    BigMsg = lists:seq(1, 4096),
    ok = srow_gateway:send(Pid1, Remote, BigMsg),
    receive Msg2 ->
            ?assertEqual(BigMsg, Msg2)
    end,

    ok = srow_gateway:stop(Pid1),
    ok = srow_gateway:stop(Pid2).


repeat_msg_test() ->
    {ok, Pid1} = srow_gateway:start_link_2({127,0,0,1}, 10003),
    {ok, Pid2} = srow_gateway:start_link_2({127,0,0,1}, 10004),

    RemotePid2 = srow:remote_process({127,0,0,1}, 10004, erlang:self()),
    Size = 1024,
    Start = os:timestamp(),
    [begin
         ok = srow_gateway:send(Pid1, RemotePid2, {burger, N})
     end
     || N <- lists:seq(1, Size)],
    R = [receive {burger, I} ->
                 %% ?debugVal({I, _N}),
                 I
         end || _N <- lists:seq(1, Size)],
    End = os:timestamp(),
    ?debugVal(R),
    ?debugFmt("Duration: ~p usec, ~p msg/secs",
              [timer:now_diff(End, Start),
               Size * 1000000 / timer:now_diff(End, Start)]),
    ?assertEqual(lists:seq(1, Size), lists:sort(R)),
    ok = srow_gateway:stop(Pid1),
    ok = srow_gateway:stop(Pid2).
