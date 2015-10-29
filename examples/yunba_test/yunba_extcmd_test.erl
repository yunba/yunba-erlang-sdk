#! /usr/bin/env escript
%%! -smp enable -mnesia debug verbose -Wall -pz ../../ebin/ ../../deps/gen_logger/ebin ../../deps/goldrush/ebin ../../deps/lager/ebin ../../deps/ibrowse/ebin ../../deps/jiffy/ebin ../../deps/snowflake/ebin

-module(yunba_extcmd_test).

-include("../../include/emqttc_packet.hrl").
-include("../../include/emqttc_yunba_misc.hrl").

-export([start/0]).

start() ->
    ok = application:start(ibrowse),  %% start ibrowse to support http get/post
    ok = application:start(snowflake),

    %% register your appkey, get clientid, username and password
    {ok, {ClientId, UserName, Password}} =
        emqttc_register:register(?YUNBA_REG_URL, ?TEST_APPKEY, ?TEST_PLATFORM),

    %% get MQTT broker IP (host and port) from yunba tick service
    {Host, Port} = emqttc_broker:get_broker(?YUNBA_TICK_URL),

    %% connect MQTT broker
    {ok, Client} = emqttc:start_link([{host, Host}, {port, Port}, {client_id, ClientId},
                                      {proto_ver, ?MQTT_PROTO_V31_YUNBA},
                                      {username, UserName},
                                      {password, Password}]),
    emqttc_alias:set_alias(Client, ?TEST_ALIAS),

    emqttc:subscribe(Client, ?TEST_TOPIC, ?QOS_0),
    receive
        {suback, PacketId} ->
            io:format("Received suback of ~p~n", [PacketId])
    after
        ?TEST_TIMEOUT ->
            io:format("Waiting for suback timeout")
    end,

    {Status, Response} = emqttc_publish_extcmd:publish_extcmd_get_topic_list(Client, ?TEST_ALIAS),

    io:format("[Client publish_extcmd_get_alias] => Status = ~p, Response = ~p~n", [Status, Response]),

    emqttc:disconnect(Client).

main(_Args) ->
    start().
