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
%%% emqttc packet.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(emqttc_packet).

-author("feng@emqtt.io").

-include("emqttc_packet.hrl").

%% API
-export([protocol_name/1, type_name/1, connack_name/1, extcmd_name/1, extcmd_status_name/1]).

-export([dump/1]).

%% %% API of generationg Yunba extcmd Packet
%% -export([form_extcmd_package/2, form_extcmd_package/3]).
%%------------------------------------------------------------------------------
%% @doc Protocol name of version.
%% @end
%%------------------------------------------------------------------------------
-spec protocol_name(Ver) -> Name when
      Ver   :: mqtt_vsn(),
      Name  :: binary().
protocol_name(Ver) when Ver =:= ?MQTT_PROTO_V31; Ver =:= ?MQTT_PROTO_V311; Ver =:= ?MQTT_PROTO_V31_YUNBA ->
    proplists:get_value(Ver, ?PROTOCOL_NAMES).

%%------------------------------------------------------------------------------
%% @doc Name of MQTT packet type.
%% @end
%%------------------------------------------------------------------------------
-spec type_name(mqtt_packet_type()) -> atom().
type_name(Type) when Type > ?RESERVED andalso Type =< ?EXTCMD ->
    lists:nth(Type, ?TYPE_NAMES).

%%------------------------------------------------------------------------------
%% @doc Connack Name.
%% @end
%%------------------------------------------------------------------------------
-spec connack_name(mqtt_connack()) -> atom().
connack_name(?CONNACK_ACCEPT)       -> 'CONNACK_ACCEPT';
connack_name(?CONNACK_PROTO_VER)    -> 'CONNACK_PROTO_VER';
connack_name(?CONNACK_INVALID_ID )  -> 'CONNACK_INVALID_ID';
connack_name(?CONNACK_SERVER)       -> 'CONNACK_SERVER';
connack_name(?CONNACK_CREDENTIALS)  -> 'CONNACK_CREDENTIALS';
connack_name(?CONNACK_AUTH)         -> 'CONNACK_AUTH'.

%%------------------------------------------------------------------------------
%% @doc Yunba Extcmd Name.
%% @end
%%------------------------------------------------------------------------------
extcmd_name(?CMD_UNKOWN)             -> 'CMD_UNKOWN';
extcmd_name(?CMD_GET_ALIAS)          -> 'CMD_GET_ALIAS';
extcmd_name(?CMD_GET_ALIAS_ACK )     -> 'CMD_GET_ALIAS_ACK';
extcmd_name(?CMD_GET_TOPIC_LIST)     -> 'CMD_GET_TOPIC_LIST';
extcmd_name(?CMD_GET_TOPIC_LIST_ACK) -> 'CMD_GET_TOPIC_LIST_ACK';
extcmd_name(?CMD_GET_ALIASLIST_ACK)  -> 'CMD_GET_ALIASLIST_ACK';
extcmd_name(?CMD_PUBLISH2)           -> 'CMD_PUBLISH2';
extcmd_name(?CMD_PUBLISH2_ACK)       -> 'CMD_PUBLISH2_ACK';
extcmd_name(?CMD_GET_STATUS)         -> 'CMD_GET_STATUS';
extcmd_name(?CMD_GET_STATUS_ACK)     -> 'CMD_GET_STATUS_ACK';
extcmd_name(?CMD_RECVACK)            -> 'CMD_RECVACK'.

%%------------------------------------------------------------------------------
%% @doc Yunba Extended Command Return Codes.
%% @end
%%------------------------------------------------------------------------------
extcmd_status_name(?STATUS_CODE_OK)                  -> 'STATUS_CODE_OK';
extcmd_status_name(?STATUS_CODE_INTERNAL_SERVER_ERR) -> 'STATUS_CODE_INTERNAL_SERVER_ERR';
extcmd_status_name(?STATUS_CODE_NOT_FOUND )          -> 'STATUS_CODE_NOT_FOUND';
extcmd_status_name(?STATUS_CODE_NO_PERMMIT)          -> 'STATUS_CODE_NO_PERMMIT';
extcmd_status_name(?STATUS_CODE_NO_PARAM)            -> 'STATUS_CODE_NO_PARAM'.

%%------------------------------------------------------------------------------
%% @doc Dump packet.
%% @end
%%------------------------------------------------------------------------------
-spec dump(mqtt_packet()) -> iolist().
dump(#mqtt_packet{header = Header, variable = Variable, payload = Payload}) ->
    dump_header(Header, dump_variable(Variable, dump_payload(Payload))).

dump_header(#mqtt_packet_header{type = Type, dup = Dup, qos = QoS, retain = Retain}, S) ->
    S1 = 
    if 
        S == undefined -> <<>>;
        true -> [", ", S]
    end,
    io_lib:format("~s(Qos=~p, Retain=~s, Dup=~s~s)", [type_name(Type), QoS, Retain, Dup, S1]).

dump_variable(undefined, _) -> 
    undefined;
dump_variable(Variable, undefined) ->
    dump_variable(Variable);
dump_variable(Variable, Payload) ->
    Payload1 =
    if
        Payload == undefined -> <<>>;
        true -> [Payload]
    end,
    io_lib:format("~s, ~s", [dump_variable(Variable), Payload1]).

dump_variable(#mqtt_packet_connect{ 
                 proto_ver     = ProtoVer, 
                 proto_name    = ProtoName,
                 will_retain   = WillRetain, 
                 will_qos      = WillQoS, 
                 will_flag     = WillFlag, 
                 clean_sess    = CleanSess, 
                 keep_alive    = KeepAlive, 
                 client_id     = ClientId, 
                 will_topic    = WillTopic, 
                 will_msg      = WillMsg, 
                 username      = Username, 
                 password      = Password} ) ->
    Format =  "ClientId=~s, ProtoName=~s, ProtoVsn=~p, CleanSess=~s, KeepAlive=~p, Username=~s, Password=~s",
    Args = [ClientId, ProtoName, ProtoVer, CleanSess, KeepAlive, Username, dump_password(Password)],
    {Format1, Args1} = if 
                        WillFlag -> { Format ++ ", Will(Qos=~p, Retain=~s, Topic=~s, Msg=~s)",
                                      Args ++ [ WillQoS, WillRetain, WillTopic, WillMsg ] };
                        true -> {Format, Args}
                       end,
    io_lib:format(Format1, Args1);

dump_variable(#mqtt_packet_connack{
                 ack_flags   = AckFlags,
                 return_code = ReturnCode } ) ->
    io_lib:format("AckFlags=~p, RetainCode=~p", [AckFlags, ReturnCode]);

dump_variable(#mqtt_packet_publish{
                 topic_name = TopicName,
                 packet_id  = PacketId} ) ->
    io_lib:format("TopicName=~s, PacketId=~p", [TopicName, PacketId]);

dump_variable(#mqtt_packet_puback{
                 packet_id = PacketId } ) ->
    io_lib:format("PacketId=~p", [PacketId]);

dump_variable(#mqtt_packet_subscribe{
                 packet_id   = PacketId,
                 topic_table = TopicTable }) ->
    io_lib:format("PacketId=~p, TopicTable=~p", [PacketId, TopicTable]);

dump_variable(#mqtt_packet_unsubscribe{
                 packet_id = PacketId,
                 topics    = Topics }) ->
    io_lib:format("PacketId=~p, Topics=~p", [PacketId, Topics]);

dump_variable(#mqtt_packet_suback{
                 packet_id = PacketId,
                 qos_table = QosTable} ) ->
    io_lib:format("PacketId=~p, QosTable=~p", [PacketId, QosTable]);

dump_variable(#mqtt_packet_unsuback{
                 packet_id = PacketId } ) ->
    io_lib:format("PacketId=~p", [PacketId]);

dump_variable(PacketId) when is_integer(PacketId) ->
    io_lib:format("PacketId=~p", [PacketId]);

dump_variable(undefined) -> undefined.

dump_payload(#mqtt_extcmd_recv_message{
                extcmd = ExtCmd, status = Status, payload = Payload}) ->
    io_lib:format("[EXTCMD_RECV] => ExtCmd = ~p, Status = ~p, Payload = ~p~n", [extcmd_name(ExtCmd), extcmd_status_name(Status), Payload]);
dump_payload(Payload) when is_binary(Payload) ->
    io_lib:format("Payload = ~p", [Payload]);
dump_payload(undefined) -> undefined.

dump_password(undefined) -> undefined;
dump_password(_) -> <<"******">>.
