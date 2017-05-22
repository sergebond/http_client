-module(http_client_SUITE).
-author("srg").

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("http_client.hrl").
% etest macros
%%-include_lib ("etest/include/etest.hrl").
%%% etest_http macros
%%-include_lib ("etest_http/include/etest_http.hrl").

-define(TEST_URL, "https://httpbin.org").

all() ->
  [
    get_json_auto,
    get_json_none,
    get_json_bad
  ].

groups() ->
  [].

init_per_suite(Config) ->
  Config.

end_per_suite(Config) ->
  Config.

get_json_auto(Config) ->
  Url = ?TEST_URL ++ "/get",
  Profile =
    #http_request_profile{
      url = Url,
      method = get,
      resp_converter = auto
    },
  BinUrl = list_to_binary(Url),
  #http_response{status = 200, head = Head, body = Body } = http_client:request(Profile),

  ct:pal("get_json_auto Head ~p~nBody ~p", [Head, Body]),

  case Body of
    [{<<"args">>, _}, {<<"headers">>,_}, {<<"origin">>, _}, {<<"url">>, BinUrl}] -> Config;
    Bad ->
      ct:pal("get_json_auto FAILED Result ~p", [Bad]),
      {fail, Config}
  end.


get_json_none(Config) ->
  Url = ?TEST_URL ++ "/get",
  Profile =
    #http_request_profile{
      url = Url,
      method = get,
      resp_converter = none
    },
  BinUrl = list_to_binary(Url),
  #http_response{status = 200, head = Head, body = Body } = http_client:request(Profile),

  ct:pal("get_json_none Head ~p~nBody ~p", [Head, Body]),

  ?assertMatch(#{<<"url">> := BinUrl}, maps:from_list(hc_utils:from_json(Body))),

  case hc_utils:from_json(Body) of
    [{<<"args">>, _}, {<<"headers">>,_}, {<<"origin">>, _}, {<<"url">>, BinUrl}] -> Config;
    Bad ->
      ct:pal("get_json_none FAILED Result ~p", [Bad]),
      {fail, Config}
  end.


get_json_bad(Config) ->
  Url = ?TEST_URL ++ "/get",
  Profile =
    #http_request_profile{
      url = Url,
      method = get,
      resp_converter = xml
    },
  BinUrl = list_to_binary(Url),

  #http_response{status = 200, head = Head, body = Body } = http_client:request(Profile),

  ct:pal("Head ~p~nBody ~p", [Head, maps:from_list(Body)]),

  ?assertMatch(#{<<"url">> := BinUrl}, maps:from_list(Body)),

  Config.