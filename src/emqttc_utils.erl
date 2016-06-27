%%%-------------------------------------------------------------------
%%% @author Zhengyinyong
%%% @copyright (C) 2015, Yunba
%%% @doc
%%%
%%% @end
%%% Created : 26. 五月 2015 下午7:57
%%%-------------------------------------------------------------------
-module(emqttc_utils).
-author("Zhengyinyong").

%% The following API is mostly from elogic project

-export([http_post/2, http_post/3, make_sure_binary/1, http_get/1, http_get/2]).

-define(HTTP_TIMEOUT, 30000).

http_get(URL) ->
    http_get(URL, ?HTTP_TIMEOUT).

http_get(URL, Timeout) ->
    case http_get_request(URL, Timeout) of
        {error, connection_closing} ->   %% if nginx, will close client after requests number exceed keepalive_request
            io:format("Http get failed by server closing connection, retry ~p", [URL]),
            http_get_request(URL, Timeout);
        Else ->
            Else
    end.

http_post(URL, Content) ->
    http_post(URL, Content, ?HTTP_TIMEOUT).

http_post(URL, Content, Timeout) ->
    case http_post_request(URL, Content, Timeout) of
        {error, connection_closing} ->
            io:format("Http post failed by server closing connection, retry ~p ~p~n", [URL, Content]),
            http_post_request(URL, Content, Timeout);
        Else ->
            Else
    end.

make_sure_binary(Data) ->
    if
        is_list(Data) ->
            list_to_binary(Data);
        is_integer(Data) ->
            integer_to_binary(Data);
        is_atom(Data) ->
            atom_to_binary(Data, latin1);
        true ->
            Data
    end.

%% Internal API
http_post_request(URL, Content, Timeout) ->
    try ibrowse:send_req(URL,[{"Content-type", "application/json"}],post, Content, [], Timeout) of
        {ok, ReturnCode, _Headers, Body} ->
            case ReturnCode of
                "200" ->
                    %io:format("http post client ~p succeed ~p", [URL, Body]),
                    {ok, Body};
                _Other ->
                    io:format("http post client ~p not return 200 ~p ~p~n", [URL, Content, _Other]),
                    {error, Body}
            end;
        {error, connection_closing} ->
            {error, connection_closing};
        {error, REASON} ->
            io:format("http post client ~p failed ~p ~p~n", [URL, Content, REASON]),
            {error, <<"http failed">>}
    catch
        Type:Error ->
            io:format("http post client ~p failed ~p ~p:~p~n", [URL, Content, Type, Error]),
            {error, <<"http failed">>}
    end.

http_get_request(URL, Timeout) ->
    try ibrowse:send_req(URL,[{"Accept", "application/json"}],get, [], [], Timeout) of
        {ok, ReturnCode, _Headers, Body} ->
            case ReturnCode of
                "200" ->
                    io:format("http get client ~p succeed ~p", [URL, Body]),
                    {ok, Body};
                _Other ->
                    io:format("http get client ~p not return 200 ~p~n", [URL, _Other]),
                    {error, Body}
            end;
        {error, connection_closing} ->
            {error, connection_closing};
        {error, REASON} ->
            io:format("http get client ~p failed ~p~n", [URL, REASON]),
            {error, <<"http failed">>}
    catch
        Type:Error ->
            io:format("http get client ~p failed ~p:~p~n", [URL, Type, Error]),
            {error, <<"http failed">>}
    end.
