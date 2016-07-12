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
-export([get_broker/4]).

%% deprecated API
-export([get_broker/1, get_broker/2]).

get_broker(Type, RequestHost, RequestPort, Request) ->
    case Type of
        http ->
            request_ticket_by_http(RequestHost, RequestPort, Request);
        tcp ->
            request_ticket_by_direct_tcp(RequestHost, RequestPort, Request);
        _ ->
            {error, invalid_network_request_protocol}
    end.

request_ticket_by_http(RequestHost, RequestPort, Request) ->
    #request_ticket_args {
        appkey = Appkey,
        networktype = NetworkType,
        networkoperator = NetworkOperator,
        sdk_version = SDKVerison,
        clientid = ClientId,
        type = Type
    } = Request,
    PostContent = jiffy:encode({[
        {<<"a">>, emqttc_utils:to_bin(Appkey)},
        {<<"n">>, emqttc_utils:to_bin(NetworkType)},
        {<<"o">>, emqttc_utils:to_bin(NetworkOperator)},
        {<<"v">>, emqttc_utils:to_bin(SDKVerison)},
        {<<"c">>, emqttc_utils:to_bin(ClientId)},
        {<<"t">>, emqttc_utils:to_bin(Type)}
    ]}),
    TickURL = "http://" ++ emqttc_utils:to_str(RequestHost) ++  ":" ++ emqttc_utils:to_str(RequestPort),
    case emqttc_utils:http_post(TickURL, PostContent) of
        {ok, Body} ->
            BodyBin = emqttc_utils:to_bin(Body),
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

request_ticket_by_direct_tcp(RequestHost, RequestPort,  Request) ->
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

%%%===================================================================
%%% Deprecated functions
%%%===================================================================
get_broker(TickURL) ->
    case emqttc_utils:http_post(TickURL, <<>>) of
        {ok, Body} ->
            BodyBin = emqttc_utils:to_bin(Body),
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
            BodyBin = emqttc_utils:to_bin(Body),
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
        Other ->
            Other
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
parse_broker_ip(Broker) when is_list(Broker) ->
    Broker1 = string:substr(Broker, string:len("tcp://") + 1, string:len(Broker)),
    [Host, Port] = string:tokens(Broker1, ":"),
    {Port1, _Rest} = string:to_integer(Port),
    {Host, Port1}.

parse_tcp_response(Data) ->
    try
        <<Version:8, Length:16, Rest1/binary>> = Data,
        case (Version =:= ?YUNBA_TICKET_TCP_DEFAULT_VERSION) andalso (size(Rest1) >= Length) of
            true ->
                <<JsonData:Length/binary, _/binary>> = Rest1,
                {JsonData1} = jiffy:decode(JsonData),
                Broker = proplists:get_value(<<"c">>, JsonData1),
                case Broker of
                    undefined ->
                        {error, parse_tcp_response_failed};
                    _ ->
                        {ok, parse_broker_ip(emqttc_utils:to_str(Broker))}
                end;
            false ->
                {error, parse_tcp_response_failed}
        end
    catch
        _Type:_Error ->
            {error, parse_tcp_response_failed}
    end.

form_tcp_data(Request) ->
    #request_ticket_args {
        appkey = Appkey,
        networktype = NetworkType,
        networkoperator = NetworkOperator,
        sdk_version = SDKVerison,
        clientid = ClientId,
        type = Type
    } = Request,
    JsonData = jiffy:encode({[
        {<<"a">>, emqttc_utils:to_bin(Appkey)},
        {<<"n">>, emqttc_utils:to_bin(NetworkType)},
        {<<"o">>, emqttc_utils:to_bin(NetworkOperator)},
        {<<"v">>, emqttc_utils:to_bin(SDKVerison)},
        {<<"c">>, emqttc_utils:to_bin(ClientId)},
        {<<"t">>, emqttc_utils:to_bin(Type)}
    ]}),
    Length = size(JsonData),
    <<?YUNBA_TICKET_TCP_DEFAULT_VERSION:8, Length:16, JsonData/binary>>.