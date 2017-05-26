-module(http_client).
-author("srg").
-include("http_client.hrl").

%% API
-export([
  request/1,
  request/2,
  request/3 ]).

-spec request(#http_request_profile{}) -> #http_response{}|{error, term()}.
request(Profile) ->
  request("", [], Profile).

-spec request(term(), #http_request_profile{}) -> #http_response{}|{error, term()}.
request(Body, Profile) ->
  request(Body, "", Profile).

-spec request(term(), list()|binary(), #http_request_profile{}) -> #http_response{}|{error, term()}.
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

get_params(Url, Body, #http_request_profile{method = Method, headers = Head, content_type = CT, charset = CS}) when Method == post; Method == put; Method == patch   ->
  ContentType = get_content_type(CT, CS),
  SerializedBody =
    try serialize_body(CT, Body) of
      {error, Reason} -> error_mess(Reason);
      Result -> Result
    catch
      _:_ ->
        error_mess("Could not serialize '~p' body ~n~p ", [CT, Body])
    end,
  {Url, Head, ContentType, SerializedBody}.

-spec serialize_body(string(), list()|proplists:proplist()) -> {ok, term()}|{error, term()}.
serialize_body(_, []) -> <<>>;
serialize_body(_CT, Body) when is_binary(Body) -> Body;
serialize_body("application/json", List) when is_list(List) ->
  case hc_utils:to_json(List) of
    <<"error json encode">> ->
      {error, <<"Error json encode">>};
    Body when is_binary(Body) -> Body
  end;
serialize_body("application/xml", Tuple) when is_tuple(Tuple) ->
  case Tuple of
    {xml, _Version, _Encoding, _RootElement} ->
      exomler:encode_document(Tuple);
    {_Tag, _Attrs, _Content} = RE ->
      exomler:encode_document({xml,'1.0',utf8, RE });
    _ ->
      { error, << "What a fuck have you just passed. Wrong xml term, mothefucker!!!" >> }
  end;
serialize_body("application/x-www-form-urlencoded", List) when is_list(List) ->
  hc_utils:join_form(List);
%%encode("multipart/form-data", List) -> ok. %% @todo
serialize_body(_, List) when is_list(List) ->
  {error, <<"Unknown content type">>};
serialize_body(Ct, Body) ->
  error_mess("Couldn't serialize body ~p into ~p", [Body, Ct]).

-spec get_content_type(string(), string()) -> nonempty_string()|no_return().
get_content_type(CT0, Charset) when is_list(CT0), is_list(Charset) ->
  CT0 ++ "; " ++ Charset;
get_content_type(_,_) ->
  error_mess(<< "'ContentType' and 'Charset' must be a strings" >> ).

-spec get_url(string(), proplists:proplist()|[]|binary()) -> nonempty_string()|no_return().
get_url([], Url0) -> Url0;
get_url(QS, Url0) when is_list(Url0), is_list(QS) ->
  Url0 ++ "?" ++ binary_to_list(hc_utils:join_form(QS));
get_url(QS, Url0) when is_list(Url0), is_binary(QS) ->
  Url0 ++ "?" ++ binary_to_list(QS);
get_url(_,_) ->
  error_mess( <<"'Url' must be a string">> ).

req(Method, Params, #http_request_profile{attempts = Attempts} = Profile) ->
  ok = application:ensure_started(inets),
  {ok, _Apps1} = application:ensure_all_started(ssl),
  try_req(Method, Params, Profile, Attempts, []).

try_req(_Method, _Params, _Profile, 0, Errors) ->
  throw({error, unicode:characters_to_binary( io_lib:format("~p", [Errors]) )}); %% @todo change somehow consist of error

try_req(Method, Params, #http_request_profile{options = Opts, http_options = Hopts, attempts = Attempts, delay = Delay0} = Profile, AttemptsRemain, Errors) ->
  Resp =  httpc:request(Method, Params, Hopts, Opts),
  case Resp of
    {ok, {{_HTTPVer, Status, _StatusPhrase}, Head, Body}} ->
      #http_response{ status = Status, head = Head, body = Body };
    {error, Error} -> %% @todo logging
      Delay = (Attempts - AttemptsRemain) * 2 * Delay0,
      timer:sleep(Delay),
      io:format("+++++Doing attempt ~p", [Attempts - AttemptsRemain]), %% @todo logging
      try_req(Method, Params, Profile, AttemptsRemain - 1, [Error|Errors]) %% @todo logging
  end.

resolve_response(#http_response{status = 200, head = Head, body = Body} = Resp, #http_request_profile{resp_converter = Converter}) ->
  Resp#http_response{body = resp_body_as_term(Converter, Body, Head)};

resolve_response(#http_response{status = 302, head = Head}, #http_request_profile{url = Url}) -> %% todo Another 300....
  Location0 = hc_utils:get_value("location", Head),
  case Location0 of
    "/" ++ _Rest ->
      Host = get_host_from_url(Url),
      Host ++ Location0;
    "http" ++  _ -> Location0
  end;

resolve_response(#http_response{status = Status, body = Body} = Resp, _) when Status >= 400 -> %% todo Another 400....500...
  Resp#http_response{body = Body}.

resp_body_as_term(auto, BinaryBody, Head) ->
  ContentType = hc_utils:get_value("content-type", Head),
  ConvertFrom =
    case ContentType of
      "application/json" ++ _ -> json; %% todo text/json
      "application/xml" ++ _ -> xml; %% todo text/xml
      "application/x-www-form-urlencoded" ++ _ -> 'x-form';
      _ -> error_mess( "Unknown content type ~p", [ContentType])
    end,
  resp_body_as_term(ConvertFrom, BinaryBody, Head);
resp_body_as_term(ConvertFrom, BinaryBody, _) ->
  try deserialize(ConvertFrom, BinaryBody) of
    {error, Message} -> error_mess(Message);
    Result -> Result
  catch
    _:_ -> error_mess("Could not deserialize ~p ~n~p ", [ConvertFrom, BinaryBody])
  end.

-spec deserialize(format(), binary()) -> list()|{error, term()}.
deserialize(json, Body) ->
  hc_utils:from_json(Body);
deserialize(xml, Body) ->
  exomler:decode(Body);
deserialize('x-form', Body) ->
  hc_utils:x_www_form_urlencoded(Body);
deserialize(multipart, _Body) ->
  {error, <<"Put your fu*ing multipart into the asshole, moron! Write code for this shit first!">>};
deserialize(none, Body) ->
  Body;
deserialize(_Unknown, _Body) ->
  {error, <<"Unknown converter">>}.

get_host_from_url(Url) when is_list(Url) ->
  Res = http_uri:parse(Url),
  case Res of
    {ok,{Scheme, _UserInfo, Host, _Port, _Path, _Query}} ->
      << (hc_utils:to_bin(Scheme))/binary, "://", (hc_utils:to_bin(Host))/binary >>;
    _ -> error
  end.

%%----------------------------------------------------------------------------------------------------------------------
%%                  ERRORS
%%----------------------------------------------------------------------------------------------------------------------
-spec error_mess(binary()) -> no_return().
error_mess(Message) when is_binary(Message) ->
  throw({error, Message}).
-spec error_mess(binary(), list()) -> no_return().
error_mess( Message, Params) when is_list(Message), is_list(Params) ->
  ErrString = lists:flatten(io_lib:format(Message, Params)),
%%  lager:error(ErrString), %% @todo
  throw({error, list_to_binary(ErrString)}).