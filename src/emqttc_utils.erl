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

-export([http_post/2, http_post/3, http_get/1, http_get/2,
         to_bin/1, to_str/1, to_int/1, to_atom/1]).

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

to_bin(Data) ->
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

to_str(Data) ->
    if
        is_binary(Data) ->
            binary_to_list(Data);
        is_integer(Data) ->
            integer_to_list(Data);
        is_atom(Data) ->
            atom_to_list(Data);
        true ->
            Data
    end.

to_int(Data) ->
    if
        is_binary(Data) ->
            binary_to_integer(Data);
        is_list(Data) ->
            list_to_integer(Data);
        is_integer(Data) ->
            Data;
        true ->
            Data
    end.

to_atom(Data) ->
    if
        is_binary(Data) ->
            binary_to_atom(Data, utf8);
        is_list(Data) ->
            list_to_atom(Data);
        is_atom(Data) ->
            Data;
        true ->
            Data
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
http_post_request(URL, Content, Timeout) ->
    try ibrowse:send_req(URL,[{"Content-type", "application/json"}],post, Content, [], Timeout) of
        {ok, ReturnCode, _Headers, Body} ->
            case ReturnCode of
                "200" ->
                    {ok, Body};
                _Other ->
                    io:format("http post client ~p not return 200 ~p ~p~n", [URL, Content, _Other]),
                    {error, Body}
            end;
        {error, connection_closing} ->
            {error, connection_closing};
        {error, _REASON} ->
            {error, <<"http failed">>}
    catch
        _Type:_Error ->
            {error, <<"http failed">>}
    end.

http_get_request(URL, Timeout) ->
    try ibrowse:send_req(URL,[{"Accept", "application/json"}],get, [], [], Timeout) of
        {ok, ReturnCode, _Headers, Body} ->
            case ReturnCode of
                "200" ->
                    {ok, Body};
                _Other ->
                    {error, Body}
            end;
        {error, connection_closing} ->
            {error, connection_closing};
        {error, _REASON} ->
            {error, <<"http failed">>}
    catch
        _Type:_Error ->
            {error, <<"http failed">>}
    end.
