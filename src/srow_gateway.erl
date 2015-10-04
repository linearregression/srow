%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <kota@basho.com>
%%% @copyright (C) 2015, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created :  4 Oct 2015 by UENISHI Kota <kota@basho.com>
%%%-------------------------------------------------------------------
-module(srow_gateway).

-behaviour(gen_server).

%% API
-export([start_link/2, send/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CHUNK_SIZE, 4096).

-ifdef(TEST).
-export([start_link_2/2, send/3]).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {
          host :: inet:ip_address(),
          port :: inet:port_number(),
          socket :: inet:socket(),
          recv_buf :: ets:tid(),
          total_msg = 0 ::non_neg_integer(),
          total_chunk = 0 ::non_neg_integer()
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Host, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Host, Port], []).

start_link_2(Host, Port) ->
    gen_server:start_link(?MODULE, [Host, Port], []).

send(Dept, Msg) ->
    send(?MODULE, Dept, Msg).

send(Pid, Dept, Msg) ->
    Binary = erlang:term_to_binary(Msg),
    ByteSize = erlang:byte_size(Binary),
    NumChunks = case ByteSize rem ?CHUNK_SIZE of
                    0 -> ByteSize div ?CHUNK_SIZE;
                    _ -> (ByteSize div ?CHUNK_SIZE) + 1
                end,
    RemoteHost = srow:remote_host(Dept),
    RemotePid = srow:remote_pid(Dept),
    MsgId = erlang:make_ref(),
    {ok, Socket} = gen_server:call(Pid, get_socket),
    send_chunks(Socket, RemoteHost, MsgId, RemotePid, 0, NumChunks, Binary).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host, Port]) ->
    {ok, Socket} = gen_udp:open(Port, [{ip, Host}, {active, once}, binary]),
    RecvBuf = ets:new(?MODULE, [private, bag]),
    {ok, #state{host=Host, port=Port, socket=Socket, recv_buf=RecvBuf}}.

%% @private
handle_call(get_socket, _From, #state{socket=Socket} = State) ->
    {reply, {ok, Socket}, State};
handle_call(Msg, From, State) ->
    ?debugVal(Msg),
    {stop, {unknown_msg, From, Msg}, State}.

%% @private
handle_cast(stop, State) ->
    {stop, normal, State}.

%% @private
handle_info({udp, Socket, IP, InPortNo, Packet},
            #state{recv_buf=RecvBuf, socket=Socket,
                   total_chunk = TotalChunks} = State) ->
    << Len:16, Header:Len/binary, Chunk/binary>> = Packet,

    case binary_to_term(Header) of
        {_, DestPid, 0, 1} -> %% Optimization to skip ets, not necessary
            Msg = binary_to_term(Chunk),
            DestPid ! Msg;

        {MsgId, DestPid, SeqNum, NumChunks} ->
            Key = {IP, InPortNo, MsgId, DestPid},
            %% We'll need garbage collection someday
            true = ets:insert(RecvBuf, {Key, {SeqNum, NumChunks, Chunk}}),
            case ets:lookup(RecvBuf, Key) of
                Chunks when length(Chunks) =:= NumChunks ->
                    Msg = assemble_chunks(Chunks),
                    %% ?debugVal({Key, SeqNum, NumChunks}),
                    %% ?debugVal(DestPid),
                    %% ?debugVal(self()),
                    DestPid ! Msg,
                    ets:delete(RecvBuf, Key);
                Other ->
                    ?debugVal(length(Other)),
                    ok
            end
    end,
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{total_chunk=TotalChunks+1}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, #state{socket = Socket, total_chunk=TC} = _State) ->
    ?debugVal(TC),
    ok = gen_udp:close(Socket),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% byte_size(Header) + byte_size(Chunk) must be smaller than 65536
send_chunks(_, _, _, _, _, _, <<>>) ->
    ok;
send_chunks(Socket,
            RemoteHost, MsgId, RemotePid, Count, NumChunks, Binary0) ->
    {Binary, Rest} =
        case Binary0 of
            <<Chunk:?CHUNK_SIZE/binary, Rest0/binary>> ->
                {Chunk, Rest0};
            _ ->
                {Binary0, <<>>}
        end,

    Header = term_to_binary({MsgId, RemotePid, Count, NumChunks}),
    IoData = [<< (byte_size(Header)):16 >>, Header, Binary],
    {Host, Port} = RemoteHost,

    %% I don't understand why we need this before sending msg,
    %% otherwise we lose some messages. I know there're no guarantee
    %% on UDP, but even when using loopback device it drops.
    timer:sleep(1),

    ok = gen_udp:send(Socket, Host, Port, IoData),

    send_chunks(Socket, RemoteHost, MsgId, RemotePid, Count+1,
                NumChunks, Rest).

assemble_chunks(Chunks) ->
    assemble_chunks(lists:reverse(lists:sort(Chunks)), <<>>).

assemble_chunks([], Binary) ->
    binary_to_term(Binary);
assemble_chunks([{_Key, {_Count, _NumChunks, Chunk}}|T], Binary) ->
    assemble_chunks(T, <<Chunk/binary, Binary/binary>>).
