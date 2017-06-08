-module(http_client_SUITE).
-author("srg").

%% WARNING!!! service for testing http clients https://httpbin.org is used

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("http_client.hrl").

-define(TEST_URL, "https://httpbin.org").

all() ->
  [
    {group, get},
    {group, post},
    {group, response_decode},
    {group, http_codes},
    {group, errors}
  ].

groups() ->
  [
%%    {all, [parallel], [
%%      {group, get},
%%      {group, post},
%%      {group, response_decode},
%%      {group, http_codes},
%%      {group, errors}
%%    ]},
    {get, [parallel],
      [
        get_json_auto,
        get_json_none,
        get_json_query_string,
        get_json_headers,
        get_json_bad
      ]},
    {post, [parallel],
      [
        post_json,
        post_xml,
        post_x_form,
        post_binary
      ]},
    {response_decode, [parallel],
      [
        post_waiting_response_xml
      ]},
    {http_codes, [parallel],
      [
        '405' %% @todo codes: redirect etc
      ]},
    {errors, [parallel],
      [
        wrong_body_serialize,
        several_attempts_with_timeout_err
      ]}
  ].

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
    [{<<"args">>, _}, {<<"headers">>,_}, {<<"origin">>, _}, {<<"url">>, BinUrl}] -> ok;
    Bad ->
      ct:pal("get_json_auto FAILED Result ~p", [Bad]),
      {fail, <<"fail">>}
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
  ?assertMatch(#{<<"url">> := BinUrl}, maps:from_list(eutils:from_json(Body))),
  case eutils:from_json(Body) of
    [{<<"args">>, _}, {<<"headers">>,_}, {<<"origin">>, _}, {<<"url">>, BinUrl}] -> Config;
    Bad ->
      ct:pal("get_json_none FAILED Result ~p", [Bad]),
      {fail, <<"fail">>}
  end.

get_json_query_string(Config) ->
  Url = ?TEST_URL ++ "/get",
  Profile =
    #http_request_profile{
      url = Url,
      method = get,
      resp_converter = json
    },
  QueryString = [
    {<<"aoo">>, <<"1">>},
    {<<"bar">>, <<"2">>},
    {<<"baz">>, <<"qwerty">>}
  ],
  #http_response{status = 200, head = Head, body = Body } = http_client:request(_BodyO = [], QueryString, Profile),
  BinUrl = list_to_binary(Url ++ "?" ++ binary_to_list(eutils:join_form(QueryString))),
  ct:pal("get_query_string Head ~p~nBody ~p", [Head, Body]),
  case Body of
    [{<<"args">>, QueryString}, {<<"headers">>,_}, {<<"origin">>, _}, {<<"url">>, BinUrl}] -> Config;
    Bad ->
      ct:pal("get_json_query_string FAILED Result ~p", [Bad]),
      {fail, <<"fail">>}
  end.

get_json_headers(Config) ->
  Url = ?TEST_URL ++ "/get",
  Profile =
    #http_request_profile{
      url = Url,
      method = get,
      resp_converter = json,
      headers = [{header1, value1},
        {<<"header2">>, <<"value2">>},
        {"header3", "value3"}]
    },
  QueryString = [
    {<<"aoo">>, <<"1">>},
    {<<"bar">>, <<"2">>},
    {<<"baz">>, <<"qwerty">>}
  ],
  #http_response{status = 200, head = Head, body = Body } = http_client:request(_BodyO = [], QueryString, Profile),
  BinUrl = list_to_binary(Url ++ "?" ++ binary_to_list(eutils:join_form(QueryString))),
  ct:pal(" Head ~p~nBody ~p", [Head, Body]),
  ExpectedHead = [{<<"Connection">>,<<"close">>},
    {<<"Header1">>,<<"value1">>},
    {<<"Header2">>,<<"value2">>},
    {<<"Header3">>,<<"value3">>},
    {<<"Host">>,<<"httpbin.org">>}],
  case Body of
    [{<<"args">>, QueryString}, {<<"headers">>, ExpectedHead}, {<<"origin">>, _}, {<<"url">>, BinUrl}] -> Config;
    Bad ->
      ct:pal("get_json_headers FAILED Result ~p", [Bad]),
      {fail, <<"fail">>}
  end.

get_json_bad(Config) ->
  Url = ?TEST_URL ++ "/get",
  Profile =
    #http_request_profile{
      url = Url,
      method = get,
      resp_converter = xml %% Bad format
    },
  Resp = http_client:request(Profile),
  ct:pal("Resp is ~p", [Resp]),
  case Resp of
    { error, << "Could not deserialize xml", _/binary >> } -> Config;
    Bad ->
      ct:pal("get_json_bad FAILED Result ~p", [Bad]),
      {fail, <<"fail">>}
  end.



%% POST
%%______________________________________________________________________________________________________________________
post_json(Config) ->
  Url = ?TEST_URL ++ "/post",
  Profile =
    #http_request_profile{
      url = Url,
      method = post,
      content_type = "application/json"
    },

  ReqBody = [{<<"aoo">>, <<"q">>}, {<<"bar">>, <<"w">>}, {<<"baz">>, <<"w">>}],

  #http_response{status = 200, head = _Head, body = RespBody } = http_client:request(ReqBody, Profile),
  ct:pal("post_json ~nResponse Body ~p", [RespBody]),
  JsonBody = eutils:to_json( ReqBody ),
  BinUrl = list_to_binary(Url),
  case maps:from_list(RespBody) of
    #{<<"data">> := JsonBody  , <<"json">> := ReqBody, <<"url">> := BinUrl } -> Config;
    Bad ->
      ct:pal("post_json_failed FAILED Result ~p", [Bad]),
      {fail, <<"fail">>}
  end.

post_xml(Config) ->
  Url = ?TEST_URL ++ "/post",
  Profile =
    #http_request_profile{
      url = Url,
      method = post,
      content_type = "application/xml"
    },
  Body = {<<"html">>, [], [<<"TextBefore">>, {<<"head">>, [], [<<"Body">>]}, <<"TextAfter">>]},
  #http_response{status = 200, head = _Head, body = RespBody } = http_client:request(Body, Profile),
  ct:pal("post_xml ~nResponse Body ~p", [RespBody]),
  XmlBody = exomler:encode_document( {xml,'1.0',utf8, Body } ),
  ct:pal("XmlBody ~p", [XmlBody]),
  BinUrl = list_to_binary(Url),
  case maps:from_list(RespBody) of
    #{<<"data">> := XmlBody, <<"url">> := BinUrl } -> Config;
    Bad ->
      ct:pal("post_xml FAILED Result ~p", [Bad]),
      {fail, <<"fail">>}
  end.

post_x_form(Config) ->
  Url = ?TEST_URL ++ "/post",
  Profile =
    #http_request_profile{
      url = Url,
      method = post,
      content_type = "application/x-www-form-urlencoded"
    },
  ReqBody = [{<<"aoo">>, <<"q">>}, {<<"bar">>, <<"w">>}, {<<"baz">>, <<"w">>}],
  #http_response{status = 200, head = _Head, body = RespBody } = http_client:request(ReqBody, Profile),
  ct:pal("post_x_form ~nResponse Body ~p", [RespBody]),
  BinUrl = list_to_binary(Url),
  case maps:from_list(RespBody) of
    #{ <<"form">> := ReqBody, <<"url">> := BinUrl } -> Config;
    Bad ->
      ct:pal("post_x_form FAILED Result ~p", [Bad]),
      {fail, Config}
  end.

post_binary(Config) ->
  Url = ?TEST_URL ++ "/post",
  Profile =
    #http_request_profile{
      url = Url,
      method = post
    },
  Body = <<"Some binary body">>,
  #http_response{status = 200, head = _Head, body = RespBody } = http_client:request(Body, Profile),
  ct:pal("post_binary ~nResponse Body ~p", [RespBody]),
  BinUrl = list_to_binary(Url),
  case maps:from_list(RespBody) of
    #{ <<"data">> := Body, <<"url">> := BinUrl } -> Config;
    Bad ->
      ct:pal("post_binary FAILED Result ~p", [Bad]),
      {fail, Config}
  end.

%% RESPONSE DECODE
%%______________________________________________________________________________________________________________________

post_waiting_response_xml(Config) ->
  Url = ?TEST_URL ++ "/xml",
  Profile =
    #http_request_profile{
      url = Url,
      method = get
    },

  #http_response{status = 200, head = _Head, body = RespBody } = http_client:request( Profile),

  try exomler:encode(RespBody) of
    Encoded when is_binary(Encoded) -> Config
  catch
    _:_ ->
    ct:pal("post_waiting_response_xml FAILED Result ~p", [RespBody]),
  {fail, <<"Fail">>}
  end.


%% HTTP_CODES
%%______________________________________________________________________________________________________________________
'405'(Config) ->
  Url = ?TEST_URL ++ "/xml",
  Profile =
    #http_request_profile{
      url = Url,
      method = post
    },
  #http_response{status = 405, head = _Head, body = RespBody } = http_client:request(Profile),
  case RespBody of
    RespBody when is_binary(RespBody) -> Config;
    _ -> ct:pal("405 FAILED Result ~p", [RespBody]),
      {fail, <<"Fail">>}
  end.

%% ERRORS
%%______________________________________________________________________________________________________________________
wrong_body_serialize(Config) ->
  Url = ?TEST_URL ++ "/post",
  Profile =
    #http_request_profile{
      url = Url,
      method = post
    },
  Body = {1, 2, 3},
  Resp = http_client:request(Body, Profile),
  case Resp of
    {error,<<"Could not serialize '\"text/plain\"' body \n{1,2,3} ">>} -> Config;

    _ -> ct:pal("wrong_body_serialize FAILED Result ~p", [Resp]),
      {fail, <<"Fail">>}
  end.

several_attempts_with_timeout_err(Config) ->
  Url = ?TEST_URL ++ "/xml",
  Profile =
    #http_request_profile{
      url = Url,
      method = get,
      http_options = [{timeout, 1}] %% very small timeout
    },

  Expected1 = {error,
    <<"[{failed_connect,[{to_address,{\"httpbin.org\",443}},{inet,[inet],timeout}]},\n {failed_connect,[{to_address,{\"httpbin.org\",443}},{inet,[inet],timeout}]},\n {failed_connect,[{to_address,{\"httpbin.org\",443}},{inet,[inet],timeout}]}]">>},
  Expected2 = {error,<<"[timeout,timeout,timeout]">>},
  case http_client:request( Profile) of
    E when E == Expected1; E == Expected2 ->
      ct:pal("Several_attempts_with_timeout_err OK ~p", [E]),
      ok;
    Wrong ->
      ct:pal("Several_attempts_with_timeout_err FAIL ~p", [Wrong]),
      {fail, <<"Several_attempts_with_timeout_err FAIL">>}
  end.