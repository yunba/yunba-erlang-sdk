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
-define(YUNBA_TICK_URL, "http://tick.yunba.io:9999").

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

%% record about Yunba system
-record(yunba_reg_info, {clientid, username, password}).
