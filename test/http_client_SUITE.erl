-module(http_client_SUITE).
-author("srg").

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("http_client.hrl").

-define(TEST_URL, "https://httpbin.org/").

all() ->
  [].

groups() ->
  [].

init_per_suite(Config) ->
  Config.

end_per_suite(Config) ->
  Config.


post(Config) ->
  Profile =
    #http_request_profile{
      url = ?TEST_URL ++ "/get",
      method = get
    }.