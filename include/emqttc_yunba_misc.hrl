%%%-------------------------------------------------------------------
%%% @author Zhengyinyong
%%% @copyright (C) 2015, Yunba
%%% @doc
%%%
%%% @end
%%% Created : 26. 五月 2015 下午7:57
%%%-------------------------------------------------------------------
-author("Zhengyinyong").

%% The Yunba Network domain name
-define(YUNBA_REG_URL, "http://reg.yunba.io:8383/device/reg/").
-define(YUNBA_RESTFUL_URL, "http://rest.yunba.io:8080").
-define(YUNBA_TICK_URL, "http://tick.yunba.io:9999").

-define(YUNBA_TICKET_TCP_HOST, "tick-t.yunba.io").
-define(YUNBA_TICKET_TCP_PORT, 9977).
-define(YUNBA_TICKET_HTTP_HOST, "tick.yunba.io").
-define(YUNBA_TICKET_HTTP_PORT, 9999).
-define(YUNBA_DIRECT_TCP_CONNECT_DEFAULT_VERSION, 1).

-define(YUNBA_REG_TCP_HOST, "reg-t.yunba.io").
-define(YUNBA_REG_TCP_PORT, 9944).
-define(YUNBA_REG_HTTP_HOST, "reg.yunba.io").
-define(YUNBA_REG_HTTP_PORT, 8383).

-record(request_ticket_args, {
    appkey,
    networktype,
    sdk_version,
    networkoperator,
    clientid,
    type
}).

-record(request_reg_args, {
    appkey,
    platform
}).

%% Test Data
-define(TEST_CLIENTID, <<"0000000005-000000482068">>).
-define(TEST_USERNAME, <<"2424032683041115008">>).
-define(TEST_PASSWORD, <<"40dcef8a92e04">>).
-define(TEST_APPKEY,   <<"5335292c44deccab56399e4f">>).
-define(TEST_PLATFORM, 2).

-define(TEST_TOPIC, <<"YUNBA_CLIENT_TEST">>).
-define(TEST_PAYLOAD, <<"HelloWorld42">>).
-define(TEST_ALIAS, <<"Yunba_Erlang_MQTT_Client">>).

-define(TEST_TIMEOUT, 12000).
-define(RANDOM_NUMBER_RANGE, 4294967296).
