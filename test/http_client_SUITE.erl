-module(http_client_SUITE).
-author("srg").

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("http_client.hrl").

-define(TEST_URL, "https://httpbin.org").

all() ->
  [
    get_json_auto,
    get_json_none,
    get_json_query_string,
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

get_json_query_string(Config) ->
  Url = ?TEST_URL ++ "/get",
  Profile =
    #http_request_profile{
      url = Url,
      method = get,
      resp_converter = json
    },

  QueryString = [{<<"foo">>, 1},{<<"bar">>, 2}, {<<"baz">>, qwerty}],

  #http_response{status = 200, head = Head, body = Body } = http_client:request(_BodyO = [], QueryString, Profile),

  BinUrl = list_to_binary(Url ++ "?" ++ binary_to_list(hc_utils:join_form(QueryString))),

  ct:pal("get_query_string Head ~p~nBody ~p", [Head, Body]),

  ?assertMatch(#{<<"url">> := BinUrl}, maps:from_list(Body)),

  case Body of
    [{<<"args">>, _}, {<<"headers">>,_}, {<<"origin">>, _}, {<<"url">>, BinUrl}] -> Config;
    Bad ->
      ct:pal("get_json_query_string FAILED Result ~p", [Bad]),
      {fail, Config}
  end.

get_json_bad(Config) ->
  Url = ?TEST_URL ++ "/get",
  Profile =
    #http_request_profile{
      url = Url,
      method = get,
      resp_converter = xml %% Bad format
    },
%%  BinUrl = list_to_binary(Url),

  Resp = http_client:request(Profile),

  ct:pal("Body ~p", [Resp]),

  case Resp of
    {error,
      <<"Could not deserialize xml \n<<\"{\\n  \\\"args\\\": {}, \\n  \\\"headers\\\": {\\n    \\\"Connection\\\": \\\"close\\\", \\n    \\\"Host\\\": \\\"httpbin.org\\\"\\n  }, \\n  \\\"origin\\\": \\\"212.3.124.206\\\", \\n  \\\"url\\\": \\\"https://httpbin.org/get\\\"\\n}\\n\">> ">>} -> Config;
    Bad ->
      ct:pal("get_json_query_string FAILED Result ~p", [Bad]),
      {fail, Config}
  end.

%%  Config.