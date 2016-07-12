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
-export([register/4]).

%% deprecated API
-export([register/3]).

register(Type, RequestHost, RequestPort, Request) ->
    case Type of
        http ->
            request_reg_by_http(RequestHost, RequestPort, Request);
        tcp ->
            request_reg_by_direct_tcp(RequestHost, RequestPort, Request);
        _ ->
            {error, invalid_network_request_protocol}
    end.

%%%===================================================================
%%% Deprecated API
%%%===================================================================
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
request_reg_by_http(RequestHost, RequestPort, Request) ->
    #request_reg_args {
        appkey = Appkey,
        platform = Platform
    } = Request,
    PostContent = jiffy:encode({[
        {<<"a">>, emqttc_utils:to_bin(Appkey)},
        {<<"p">>, emqttc_utils:to_bin(Platform)}
    ]}),
    RegURL = "http://" ++ emqttc_utils:to_str(RequestHost) ++  ":" ++ emqttc_utils:to_str(RequestPort) ++ "/device/reg/",
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
                    io:format("Register Yunba Broker failed ~p:~p~n", [Type, Error]),
                    {error, Error}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

request_reg_by_direct_tcp(RequestHost, RequestPort,  Request) ->
    case gen_tcp:connect(RequestHost, RequestPort, [binary, {packet, 0}, {active,false}]) of
        {ok, Socket} ->
            ok = gen_tcp:send(Socket, form_tcp_data(Request)),
            Result = case gen_tcp:recv(Socket, 0) of
                         {ok, Data} ->
                             parse_tcp_response(Data);
                         {error, Reason} ->
                             {error, Reason}
                     end,
            gen_tcp:close(Socket),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

parse_tcp_response(Data) ->
    try
        <<Version:8, Length:16, Rest1/binary>> = Data,
        case (Version =:= ?YUNBA_DIRECT_TCP_CONNECT_DEFAULT_VERSION) andalso (size(Rest1) >= Length) of
            true ->
                <<JsonData:Length/binary, _/binary>> = Rest1,
                {JsonData1} = jiffy:decode(JsonData),
                ClientId = proplists:get_value(<<"c">>, JsonData1),
                UserName = proplists:get_value(<<"u">>, JsonData1),
                Password = proplists:get_value(<<"p">>, JsonData1),
                case {ClientId, UserName, Password} of
                    {undefined, _, _} ->
                        io:format("ERROR Data = ~p~n", [Data]),
                        {error, parse_tcp_response_failed};
                    {_, undefined, _} ->
                        io:format("ERROR Data = ~p~n", [Data]),
                        {error, parse_tcp_response_failed};
                    {_, _, undefined} ->
                        io:format("ERROR Data = ~p~n", [Data]),
                        {error, parse_tcp_response_failed};
                    {_, _, _} ->
                        {ok, {ClientId, UserName, Password}}
                end;
            false ->
                io:format("ERROR Data = ~p~n", [Data]),
                {error, parse_tcp_response_failed}
        end
    catch
        _Type:_Error ->
            io:format("ERROR Data = ~p~n", [Data]),
            {error, parse_tcp_response_failed}
    end.

form_tcp_data(Request) ->
    #request_reg_args {
        appkey = Appkey,
        platform = Platform
    } = Request,
    JsonData = jiffy:encode({[
        {<<"a">>, emqttc_utils:to_bin(Appkey)},
        {<<"p">>, emqttc_utils:to_bin(Platform)}
    ]}),
    Length = size(JsonData),
    <<?YUNBA_DIRECT_TCP_CONNECT_DEFAULT_VERSION:8, Length:16, JsonData/binary>>.