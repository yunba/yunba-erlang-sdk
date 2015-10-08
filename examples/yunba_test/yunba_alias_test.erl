#! /usr/bin/env escript
%%! -smp enable -mnesia debug verbose -Wall -pz ../../ebin/ ../../deps/gen_logger/ebin ../../deps/goldrush/ebin ../../deps/lager/ebin ../../deps/ibrowse/ebin ../../deps/jiffy/ebin ../../deps/snowflake/ebin

-module(yunba_alias_test).

-include("../../include/emqttc_packet.hrl").
-include("../../include/emqttc_yunba_misc.hrl").

-export([start/0]).

start() ->
    ok = application:start(ibrowse),  %% start ibrowse to support http get/post
    ok = application:start(snowflake),

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

    %% sets client as alias
    emqttc_alias:set_alias(Client, ?TEST_ALIAS),

    %% gets client's alias
    Alias = emqttc_alias:get_alias(Client),
    io:format("Client's alias is ~p~n", [Alias]),

    %% publish to client use alias
    emqttc_alias:publish_to_alias(Client, ?TEST_ALIAS, ?TEST_PAYLOAD),

    %% waiting for receiving the message that we published
    receive
        {publish, Topic, Payload} ->
            io:format("Message Received from ~s: ~p~n", [Topic, Payload])
    after
        1000 ->
            io:format("Error: receive timeout!~n")
    end,

    %% disconnect the broker after finishing the simple test
    emqttc:disconnect(Client).

main(_Args) ->
    start().
