%%%-------------------------------------------------------------------
%%% @author Zhengyinyong
%%% @copyright (C) 2015, Yunba
%%% @doc
%%%
%%% @end
%%% Created : 26. 五月 2015 下午7:57
%%%-------------------------------------------------------------------
-module(emqttc_register).
-author("Zhengyinyong").

-include("emqttc_yunba_misc.hrl").
-include("emqttc_packet.hrl").

%% API
-export([register/3]).

register(RegURL, AppKey, Platform) ->
    PostContent = jiffy:encode({[{<<"a">>, AppKey}, {<<"p">>, Platform}]}),
    case emqttc_utils:http_post(RegURL, PostContent) of
        {ok, Body} ->
            BodyBin = emqttc_utils:to_bin(Body),
            try
                {RegInfo} = jiffy:decode(BodyBin),
                ClientId = proplists:get_value(<<"c">>, RegInfo),
                UserName = proplists:get_value(<<"u">>, RegInfo),
                Password = proplists:get_value(<<"p">>, RegInfo),
                {ok, {ClientId, UserName, Password}}
            catch
                Type:Error->
                    io:format("Register MQTT client failed ~p:~p~n", [Type, Error]),
                    {error, register_failed}
            end;
        _Else ->
            {error, register_failed}
    end.
