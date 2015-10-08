%%%-------------------------------------------------------------------
%%% @author Zhengyinyong
%%% @copyright (C) 2015, Yunba
%%% @doc
%%%
%%% @end
%%% Created : 26. 五月 2015 下午7:57
%%%-------------------------------------------------------------------
-module(emqttc_publish_extcmd).
-author("Zhengyinyong").

-include("emqttc_packet.hrl").
-include("emqttc_yunba_misc.hrl").

%% API
-export([publish_extcmd_get_alias/1, publish_extcmd_get_alias_list/2, publish_extcmd_get_topic_list/2,
         publish_extcmd_publish2/2, publish_extcmd_get_status/2]).

publish_extcmd_get_alias(Client) ->
    emqttc:publish_extcmd(Client, ?CMD_GET_ALIAS, <<>>),
    receive_loop().

publish_extcmd_get_alias_list(Client, TopicBin) when is_binary(TopicBin) ->
    emqttc:publish_extcmd(Client, ?CMD_GET_ALIASLIST, TopicBin),
    receive_loop().

publish_extcmd_get_topic_list(Client, AliasBin) when is_binary(AliasBin) ->
    emqttc:publish_extcmd(Client, ?CMD_GET_TOPIC_LIST, AliasBin),
    receive_loop().

publish_extcmd_publish2(Client, Payload) when is_binary(Payload) ->
    emqttc:publish_extcmd(Client, ?CMD_PUBLISH2, Payload),
    receive_loop().

publish_extcmd_get_status(Client, AliasBin) when is_binary(AliasBin) ->
    emqttc:publish_extcmd(Client, ?CMD_GET_STATUS, AliasBin),
    receive_loop().

%% Internal API
receive_loop() ->
    receive
        {publish_extcmd, Message} ->
                #mqtt_extcmd_recv_message {
                    extcmd = _ExtCmd,
                    status = Status,
                    len = _Len,
                    payload = Payload
                 } = Message,
                {Status, Payload};
        _Else ->
            receive_loop()
    after
        30000 ->
            io:format("Client publish_extcmd_get_alias error: receive timeout!~n"),
            ignore
    end.
