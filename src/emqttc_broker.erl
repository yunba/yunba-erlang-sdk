%%%-------------------------------------------------------------------
%%% @author Zhengyinyong
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 五月 2015 上午11:53
%%%-------------------------------------------------------------------
-module(emqttc_broker).
-author("Zhengyingyong").

-include("emqttc_yunba_misc.hrl").

%% API
-export([get_broker/1, get_broker/2]).

get_broker(TickURL) ->
    case emqttc_utils:http_post(TickURL, <<>>) of
        {ok, Body} ->
            BodyBin = emqttc_utils:make_sure_binary(Body),
            try
                {BrokerInfo} = jiffy:decode(BodyBin),
                BrokerBin = proplists:get_value(<<"c">>, BrokerInfo),
                Broker = parse_broker_ip(binary_to_list(BrokerBin)),
                {ok, Broker}
            catch
                Type:Error->
                    io:format("Register Yunba Broker failed ~p:~p~n", [Type, Error]),
                    {error, Error}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_broker(TickURL, AppKey) ->
    PostContent = jiffy:encode({[{<<"a">>, AppKey}]}),
    case emqttc_utils:http_post(TickURL, PostContent) of
        {ok, Body} ->
            BodyBin = emqttc_utils:make_sure_binary(Body),
            try
                {BrokerInfo} = jiffy:decode(BodyBin),
                BrokerBin = proplists:get_value(<<"c">>, BrokerInfo),
                Broker = parse_broker_ip(binary_to_list(BrokerBin)),
                {ok, Broker}
            catch
                Type:Error->
                    io:format("Register Yunba Broker failed ~p:~p~n", [Type, Error]),
                    {error, Error};
            end;
        Other ->
            Other
    end.

%% Internal API
parse_broker_ip(Broker) when is_list(Broker) ->
    Broker1 = string:substr(Broker, string:len("tcp://") + 1, string:len(Broker)),
    [Host, Port] = string:tokens(Broker1, ":"),
    {Port1, _Rest} = string:to_integer(Port),
    {Host, Port1}.
