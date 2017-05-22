-module(http_client).
-author("srg").
-include("http_client.hrl").

%% API
-export([
  request/1,
  request/2,
  request/3 ]).

request(Profile) ->
  request("", [], Profile).

-spec request(list()|binary(), #http_request_profile{}) -> {ok, term()}|{error, term()}.
request(Body, Profile) when is_list(Body) ->
  request(Body, "", Profile).

-spec request(list()|binary(), list()|binary(), #http_request_profile{}) -> {ok, term()}|{error, term()}.
request( Body, QueryString, Profile) when is_record(Profile, http_request_profile), is_list(QueryString) ->
  try
    make_request(Body, QueryString, Profile)
  catch
    throw:{error, Reason} ->
      {error, Reason};
    error:Reason ->
      {error, Reason, erlang:get_stacktrace()} %% @todo
  end.

make_request( Body, QueryString, #http_request_profile{method = Method, url = Url0} = Profile) ->
  Url = get_url(QueryString, Url0),
  Params = get_params(Url, Body, Profile),
  Res = req(Method, Params, Profile),
  resolve_response(Res, Profile).

get_params(Url, _Body, #http_request_profile{method = GetOrHead, headers = Head}) when GetOrHead == get; GetOrHead == head ->
  {Url, Head};

get_params(Url, Body, #http_request_profile{method = Method, headers = Head, url = Url, content_type = CT, charset = CS}) when Method == post; Method == put; Method == patch   ->
  CT = get_content_type(CT, CS),
  SerializedBody = serialize_body(CS, Body),
  {Url, Head, CT, SerializedBody}.

-spec serialize_body(string(), list()|proplists:proplist()) -> {ok, term()}|{error, term()}.
serialize_body(CT, Body) when is_binary(Body) orelse CT == "text/plain" ->
  Body;
serialize_body("application/json", List) ->
  case hc_utils:to_json(List) of
    <<"error json encode">> ->
      throw({error, <<"Error json encode">>});
    Body when is_binary(Body) -> Body
  end;
serialize_body("application/xml", List) ->
  case List of
    {xml, _Version, _Encoding, _RootElement} ->
      exomler:encode_document(List);
    {_Tag, _Attrs, _Content} = RE ->
      exomler:encode_document({xml,'1.0',utf8, RE });
    _ ->
      throw({ error, << "What a fuck have you just passed. Wrong xml term!!!" >> })
  end;
serialize_body("application/x-www-form-urlencoded", List) ->
  hc_utils:join_form(List);

%%encode("multipart/form-data", List) -> ok. %% @todo
serialize_body(_, _List) -> {error, <<"Unknown content type">>}.

-spec get_content_type(string(), string()) -> string().
get_content_type(CT0, Charset) when is_list(CT0), is_list(Charset) ->
  CT0 ++ "; " ++ Charset;
get_content_type(_,_) ->
  throw({error, << "'ContentType' and 'Charset' must be a strings" >> }).

-spec get_url(string(), proplists:proplist()|[]|binary()) -> string().
get_url([], Url0) -> Url0;
get_url(QS, Url0) when is_list(Url0), is_list(QS) ->
  Url0 ++ binary_to_list(hc_utils:join_form(QS));
get_url(QS, Url0) when is_list(Url0), is_binary(QS) ->
  Url0 ++ binary_to_list(QS);
get_url(_,_) ->
  throw({error, <<"'Url' must be a string">>}).

req(Method, Params, #http_request_profile{attempts = Attempts} = Profile) ->
  {ok, _Apps0} = application:ensure_all_started(inets),
  {ok, _Apps1} = application:ensure_all_started(ssl),
  try_req(Method, Params, Profile, Attempts, []).

try_req(_Method, _Params, _Profile, 0, Errors) ->
  throw({error, Errors}); %% @todo change somehow consist of error

try_req(Method, Params, #http_request_profile{options = Opts, http_options = Hopts, attempts = Attempts, delay = Delay} = Profile, AttemptsRemain, Errors) ->
  Resp =  httpc:request(Method, Params, Hopts, Opts),
  case Resp of
    {ok, {{_HTTPVer, Status, _StatusPhrase}, Head, Body}} ->
      #http_response{ status = Status, head = Head, body = Body };
    {error, {Error, Reas}} -> %% @todo logging
      Delay = (Attempts - AttemptsRemain) * 2 * Delay,
      timer:sleep(Delay),
      try_req(Method, Params, Profile, AttemptsRemain - 1, [{Error, Reas}|Errors]) %% @todo logging
  end.

resolve_response(#http_response{status = 200, head = Head, body = Body} = Resp, #http_request_profile{resp_converter = Converter}) ->
  Resp#http_response{body = resp_body_as_term(Converter, Body, Head)}; %% @todo

resolve_response(#http_response{status = 302, head = Head}, #http_request_profile{url = Url}) ->
  Location0 = hc_utils:get_value("location", Head),
  case Location0 of
    "/" ++ _Rest ->
      Host = get_host_from_url(Url),
      Host ++ Location0;
    "http" ++  _ -> Location0
  end.

resp_body_as_term(auto, BinaryBody, Head) ->
  ContentType = hc_utils:get_value("content-type", Head),
  ConvertFrom =
    case ContentType of
      "application/json" ++ _ -> json;
      "text/xml" ++ _ -> xml;
      "application/x-www-form-urlencoded" ++ _ -> 'x-form';
      _ -> throw({error, <<"Unknown content type">>})
    end,
  resp_body_as_term(ConvertFrom, BinaryBody, Head);
resp_body_as_term(ConvertFrom, BinaryBody, _) ->
  deserialize(ConvertFrom, BinaryBody).

deserialize(json, Body) ->
  hc_utils:from_json(Body);
deserialize(xml, Body) ->
  exomler:decode(Body);
deserialize('x-form', Body) ->
  hc_utils:x_www_form_urlencoded(Body);
deserialize(multipart, _Body) ->
  throw({error, <<"Put your fu*ing multipart into the asshole, moron! Write code for it first!">>});
deserialize(none, Body) ->
  Body;
deserialize(_Unknown, _Body) ->
  throw({error, <<"Unknown converter">>}).

get_host_from_url(Url) when is_list(Url) ->
  Res = http_uri:parse(Url),
  case Res of
    {ok,{Scheme, _UserInfo, Host, _Port, _Path, _Query}} ->
      << (hc_utils:to_bin(Scheme))/binary, "://", (hc_utils:to_bin(Host))/binary >>;
    _ -> error
  end.