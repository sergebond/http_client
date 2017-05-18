-module(http_client).
-author("srg").
-include("http_client.hrl").

-define(DELAY, 500).
-define(MAX_ATTEMPTS, 3).

%% API
-export([
  request/2,
  request/3 ]).

request(Profile, Body) when is_list(Body) ->
  request(Profile, "", Body).

-spec request(#http_client_profile{}, list(), term()) -> {ok, term()}|{error, term()}.
request(Profile, QueryString, Body0) when is_record( Profile, http_client_profile ), is_list(QueryString) ->

  #http_client_profile{
    method = Method,
    url = Url0,
    content_type = ContentType0,
    headers = Headers,
    http_options = HTTPOpts,
    options = Options
  } = Profile,

  {ok, EncodedBody} = hc_utils:encode(ContentType0, Body0), %% @todo Обработка ошибок

  ContentType = ContentType ++ "; charset=utf-8",

  Url = add_qs(Url0, QueryString),

  Res = request(Method, {Url, EncodedBody, ContentType, Headers}, HTTPOpts, Options),

  case Res of
    ok -> ok
  end.


-spec request(method(), {string(), list(), content_type(), list()}, list(), list()) -> term().
request(Method, {Url, Body, ContentType, Headers}, HtttOpts, Options ) when is_list(Url),
  is_atom(Method),
  is_binary(Body),
  is_list(ContentType),
  is_list(Options) ->

  ok = application:ensure_started(inets),
  ok = application:ensure_started(ssl),

  Params = {Url, Headers, ContentType, Body},
  do_request(Method, Params, HtttOpts, Options).

do_request(Method, Params, HTTPOpts, Opts) ->
  do_request(Method, Params, HTTPOpts, Opts, 1).

do_request(_,_,_,_, ?MAX_ATTEMPTS) ->
  throw({error, <<"Could not reach the host">>});

do_request(Method, Params, HTTPOpts, Opts, Count ) ->

  Resp =  httpc:request(Method, Params, HTTPOpts, Opts),

  case Resp of

    {ok, {{"HTTP/1.1", 200, "OK"}, Head, Body}} ->
      hc_utils:convert_resp_body(Body, Head); %% @todo

    {ok,{{"HTTP/1.1", 302, "Found"}, Head, Body}} ->
      Location0 = hc_utils:get_value("location", Head),
      Location =
        case Location0 of
          "/" ++ _Rest ->
            Url = element(1, Params),
            Host = hc_utils:get_host_from_url(Url),
            Host ++ Location0;
          "http" ++  _ -> Location0
        end,

      {ok, [{redirect, list_to_binary(Location)}]};

    {error, {_Error, _}} -> %% @todo logging
      Delay = Count * 2 * ?DELAY,
      timer:sleep(Delay),
      do_request(Method, Params, HTTPOpts, Opts, Count + 1);

    BadRes ->
%%      {error,{failed_connect,[{to_address,{"www.liqpay.com",443}},{inet,[inet],nxdomain}]}}}}
%%      {ok,{{"HTTP/1.1",302,"Found"},[{"connection","keep-alive"},{"date","Mon, 08 May 2017 08:35:23 GMT"},{"location",[104,116,116,112,115,58,47,47,119,119,119,46,108,105,113,112,97,121,46,99,111,109,47,101,110,47,99,104,101,99,107,111,117,116,47,116,104,114,111,119,95,101,114,114,111,114,47,63,101,114,114,95,100,101,115,99,114,105,112,116,105,111,110,61,208,157,208,181,32,208,191,208,181,209,128,208,181,208,180,208,176,208,189,32,208,191,208,176,209,128,208,176,208,188,208,181,209,130,209,128,32,...]},...],...}}
      BadRes
  end.



%%%% UTILS
%%%%----------------------------------------------------------------------------------------------------------------------

add_qs(Url, []) -> Url;
add_qs(Url0, Qs) when is_list(Url0) ->
  Url0 ++ binary_to_list(hc_utils:join_form(Qs)).

