%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2012-2015 eMQTT.IO, All Rights Reserved.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-----------------------------------------------------------------------------
%%% @doc
%%% emqttc message handler.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(emqttc_message).

-author("feng@emqtt.io").

%% API
-export([]).

-include("emqttc_packet.hrl").

-export([from_packet/1, to_packet/1, extcmd_message_to_packet/1]).

-export([set_flag/1, set_flag/2, unset_flag/1, unset_flag/2]).

-export([dump/1]).

%%------------------------------------------------------------------------------
%% @doc message from packet
%% @end
%%------------------------------------------------------------------------------
-spec from_packet(mqtt_packet()) -> mqtt_message() | undefined.
from_packet(#mqtt_packet{ header = #mqtt_packet_header{ type   = ?PUBLISH,
    retain = Retain,
    qos    = Qos,
    dup    = Dup },
    variable = #mqtt_packet_publish{ topic_name = Topic,
        packet_id  = PacketId },
    payload = Payload }) ->
    #mqtt_message{ msgid    = PacketId,
        qos      = Qos,
        retain   = Retain,
        dup      = Dup,
        topic    = Topic,
        payload  = Payload };

from_packet(#mqtt_packet_connect{ will_flag  = false }) ->
    undefined;

from_packet(#mqtt_packet_connect{ will_retain = Retain,
    will_qos    = Qos,
    will_topic  = Topic,
    will_msg    = Msg }) ->
    #mqtt_message{ retain  = Retain,
        qos     = Qos,
        topic   = Topic,
        dup     = false,
        payload = Msg }.

%%------------------------------------------------------------------------------
%% @doc message to packet
%% @end
%%------------------------------------------------------------------------------
-spec to_packet(mqtt_message()) -> mqtt_packet().
to_packet(#mqtt_message{ msgid   = MsgId,
    qos     = Qos,
    retain  = Retain,
    dup     = Dup,
    topic   = Topic,
    payload = Payload }) ->

    PacketId = if
                   Qos =:= ?QOS_0 -> undefined;
                   true -> MsgId
               end,

    #mqtt_packet{ header = #mqtt_packet_header { type 	 = ?PUBLISH,
        qos    = Qos,
        retain = Retain,
        dup    = Dup },
        variable = #mqtt_packet_publish { topic_name = Topic,
            packet_id  = PacketId },
        payload = Payload }.

extcmd_message_to_packet(#mqtt_extcmd_message{ msgid = MsgId, extcmd =  ExtCmd, payload = Payload, qos = _Qos}) when is_binary(Payload) ->
    Len = size(Payload),
    ExtCmdBin = <<ExtCmd:8, Len:16/integer-unsigned-big, Payload/binary>>,
    io:format("ExtCmd = ~p, Len = ~p, Payload = ~p~n", [ExtCmd, Len, Payload]),
    io:format("ExtCmdBin = ~p~n", [ExtCmdBin]),
    #mqtt_packet{ header = #mqtt_packet_header {type = ?EXTCMD, qos = ?QOS_1},
        variable = #mqtt_packet_publish {
            packet_id  = MsgId },
        payload = ExtCmdBin }.

%%------------------------------------------------------------------------------
%% @doc set dup, retain flag
%% @end
%%------------------------------------------------------------------------------
-spec set_flag(mqtt_message() ) -> mqtt_message().
set_flag(Msg) ->
    Msg#mqtt_message{dup = true, retain = true}.
-spec set_flag(atom(), mqtt_message() ) -> mqtt_message().
set_flag(dup, Msg = #mqtt_message{dup = false}) ->
    Msg#mqtt_message{dup = true};
set_flag(retain, Msg = #mqtt_message{retain = false}) ->
    Msg#mqtt_message{retain = true};
set_flag(Flag, Msg) when Flag =:= dup orelse Flag =:= retain -> Msg.

%%------------------------------------------------------------------------------
%% @doc unset dup, retain flag
%% @end
%%------------------------------------------------------------------------------
-spec unset_flag(mqtt_message() ) -> mqtt_message().
unset_flag(Msg) ->
    Msg#mqtt_message{dup = false, retain = false}.
-spec unset_flag(atom(), mqtt_message() ) -> mqtt_message().
unset_flag(dup, Msg = #mqtt_message{dup = true}) ->
    Msg#mqtt_message{dup = false};
unset_flag(retain, Msg = #mqtt_message{retain = true}) ->
    Msg#mqtt_message{retain = false};
unset_flag(Flag, Msg) when Flag =:= dup orelse Flag =:= retain -> Msg.

%%------------------------------------------------------------------------------
%% @doc dump message
%% @end
%%------------------------------------------------------------------------------
dump(#mqtt_message{msgid= MsgId, qos = Qos, retain = Retain, dup = Dup, topic = Topic}) ->
    io_lib:format("Message(MsgId=~p, Qos=~p, Retain=~s, Dup=~s, Topic=~s)",
        [ MsgId, Qos, Retain, Dup, Topic ]).
