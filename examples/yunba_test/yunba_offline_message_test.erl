#! /usr/bin/env escript
%%! -smp enable -mnesia debug verbose -Wall -pz ../../ebin/ ../../deps/gen_logger/ebin ../../deps/goldrush/ebin ../../deps/lager/ebin ../../deps/ibrowse/ebin ../../deps/jiffy/ebin ../../deps/snowflake/ebin

-module(yunba_offline_message_test).

-include("../../include/emqttc_packet.hrl").
-include("../../include/emqttc_yunba_misc.hrl").

-export([start/0]).

start() ->
    ok = application:start(ibrowse),  %% start ibrowse to support http get/post
    ok = application:start(snowflake),


    {ClientOnline, _OnlineSession} = create_client(),
    {ClientOffline, OfflineSeesion} = create_client(),

    %% subscribe the test topic
    emqttc:subscribe(ClientOnline, ?TEST_TOPIC, ?QOS_1),
    emqttc:subscribe(ClientOffline, ?TEST_TOPIC, ?QOS_1),

    2 = receive_suback_loop(0, 2),

    emqttc:disconnect(ClientOffline),

    timer:sleep(2000),

    %% publish the message
    emqttc:publish(ClientOnline, ?TEST_TOPIC, ?TEST_PAYLOAD, qos1),
    ClientOffline2 = reconnect_client(OfflineSeesion),
    receive_publish_message_loop(0, 2),

    %% unsubscribe the topic
    emqttc:unsubscribe(ClientOnline, ?TEST_TOPIC),
    emqttc:unsubscribe(ClientOffline2, ?TEST_TOPIC),

    %% disconnect the broker after finishing the test
    emqttc:disconnect(ClientOnline),
    emqttc:disconnect(ClientOffline2).

create_client() ->
    %% register your appkey, get clientid, username and password
    #yunba_reg_info{clientid = ClientId, username = UserName, password = Password} =
        emqttc_register:register(?YUNBA_REG_URL, ?TEST_APPKEY, ?TEST_PLATFORM),

    %% get MQTT broker IP (host and port) from yunba tick service
    {Host, Port} = emqttc_broker:get_broker(?YUNBA_TICK_URL),

    %% connect MQTT broker
    {ok, Client} = emqttc:start_link([{host, Host}, {port, Port}, {client_id, ClientId},
        {proto_ver, ?MQTT_PROTO_V31_YUNBA},
        {username, UserName},
        {password, Password}]),
    {Client, {ClientId, UserName, Password}}.

reconnect_client(Session) ->
    {ClientId, UserName, Password} = Session,

    %% get MQTT broker IP (host and port) from yunba tick service
    {Host, Port} = emqttc_broker:get_broker(?YUNBA_TICK_URL),

    %% connect MQTT broker
    {ok, Client} = emqttc:start_link([{host, Host}, {port, Port}, {client_id, ClientId},
        {proto_ver, ?MQTT_PROTO_V31_YUNBA},
        {username, UserName},
        {password, Password}]),

    Client.

receive_suback_loop(Counter, Accept) ->
    case Counter /= Accept of
        true ->
            receive
                {suback, _PacketId} ->
                    receive_suback_loop(Counter + 1, Accept)
            after
                12000 ->
                    undefined
            end;
        false ->
            Counter
    end.

receive_publish_message_loop(Counter, Accept) ->
    case Counter /= Accept of
        true ->
            receive
                {publish, Topic, Payload} ->
                    io:format("Message Received from ~s: ~p~n", [Topic, Payload]),
                    receive_publish_message_loop(Counter + 1, Accept)
            after
                12000 ->
                    io:format("Error: receive timeout!~n"),
                    undefined
            end;
        false ->
            Counter
    end.

main(_Args) ->
    start().
