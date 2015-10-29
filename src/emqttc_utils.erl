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

-export([http_post/2, http_post/3, make_sure_binary/1]).

-define(HTTP_TIMEOUT, 30000).

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
