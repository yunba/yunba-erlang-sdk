%%%-------------------------------------------------------------------
%%% @author Zhengyinyong
%%% @copyright (C) 2015, Yunba
%%% @doc
%%%
%%% @end
%%% Created : 26. 五月 2015 下午7:57
%%%-------------------------------------------------------------------
-module(emqttc_alias).
-author("Zhengyinyong").

-include("emqttc_yunba_misc.hrl").
-include("emqttc_packet.hrl").

%% API
-export([set_alias/2, get_alias/1, publish_to_alias/3]).

set_alias(Client, AliasName) ->
    AliasNameBin = emqttc_utils:to_bin(AliasName),
    emqttc:publish(Client, ?PUBLISH_TOPIC_FOR_SET_ALIAS, AliasNameBin, qos1),
    timer:sleep(1000).

get_alias(Client) ->
    emqttc:publish(Client, ?PUBLISH_TOPIC_FOR_GET_ALIAS, <<>>, qos1),
    receive
        {publish, Topic, Payload} ->
            case Topic =:= ?PUBLISH_TOPIC_FOR_GET_ALIAS of
                true ->
                    Payload;
                false ->
                    undefined
            end
        after
            1000 ->
                io:format("Client get_alias Error: receive timeout!~n"),
                undefined
    end.

publish_to_alias(Client, AliasName, Payload) ->
    AliasNameBin = emqttc_utils:to_bin(AliasName),
    AliasTopicBin = <<?PUBLISH_TOPIC_PREFIX_FOR_PUBLISH_TO_ALIAS/binary, AliasNameBin/binary>>,
    emqttc:publish(Client, AliasTopicBin, Payload, qos1).
