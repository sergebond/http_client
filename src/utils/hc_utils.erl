-module(hc_utils).
-author("srg").

%%%% API
%%-export([
%%  encode/2
%%]).

-export([
  to_json/1,
  from_json/1,
  get_value/2,
  get_value/3,
  config/2,
  bjoin/1,
  bjoin/2,
  to_bin/1,
  urlencode/1,
  get_unixtime/0,
  join_form/1
]).

%%----------------------------------------------------------------------
%%                        JSON ENCODE/DECODE
%%----------------------------------------------------------------------

% Term to json convertor -> binary()
to_json({ok, Data}) when is_binary(Data)->
  Data;
to_json(Data) when is_binary(Data)->
  Data;

% jsx
to_json(Data)->
  to_json_run(jiffy, Data).

% jiffy
to_json_run(jiffy, Data)->
  NewData = jiffy_encode_params(Data),
  try jiffy:encode( NewData ) of
    JSON    -> JSON
  catch _E:_Desc ->
%%    lager:error("[JSON] Error jiffy:encode ~p~n~p", [ NewData, erlang:get_stacktrace() ]) ,
    <<"error json encode">>
  end.

from_json( Text ) -> from_json( Text, <<"error_json_decode">> ).
from_json(Text, Default)->
  from_json_run(jiffy, Text, Default).


% jiffy
from_json_run(jiffy, null, Default)->
  Default;

from_json_run(jiffy, Text, Default)->
  try jiffy:decode( Text ) of
    Data  ->
      jiffy_decode_params(Data)
  catch _E:_Desc  ->
%%    lager:error("[JSON] Error jiffy:decode ~p ~n~p", [ Text, erlang:get_stacktrace() ]),
    Default
  end.

% encode
jiffy_encode_params(List = [{_, _} | _]) when is_list(List)->
  Res =
    lists:map(fun(Value) ->
      case Value of
        {Key, Val} -> {Key, jiffy_encode_param(Val)};
        _          -> jiffy_encode_params(Value)
      end
              end, List),
  {Res};

jiffy_encode_params(List) when is_list(List)->
  lists:map(fun(Value)-> jiffy_encode_params(Value) end, List);

jiffy_encode_params(Params)-> Params.


% [{},{},{}]
jiffy_encode_param(Val = [{_, _} | _])->
  jiffy_encode_params(Val);

% [[{},{},{}],[{},{},{}],[{},{},{}]]
jiffy_encode_param(Val) when is_list(Val)->
  lists:map(fun(Value)-> jiffy_encode_params(Value) end, Val);

jiffy_encode_param(Val)->
  Val.

% decode
jiffy_decode_params({List = [{_, _} | _]}) when is_list(List)->
  lists:map(fun(Value)->
    case Value of
      {Key, Val} -> {Key, jiffy_decode_param(Val)};
      _          -> jiffy_decode_params(Value)
    end
            end, List);

jiffy_decode_params(List) when is_list(List)->
  lists:map(fun(Value)-> jiffy_decode_params(Value) end, List);

jiffy_decode_params(Params)->
  Params.

% [{},{},{}]
jiffy_decode_param({Val = [{_, _} | _]})->
  jiffy_decode_params({Val});

% [[{},{},{}],[{},{},{}],[{},{},{}]]
jiffy_decode_param(Val) when is_list(Val)->
  lists:map(fun(Value)-> jiffy_decode_params(Value) end, Val);

jiffy_decode_param(Val)->
  Val.

%%----------------------------------------------------------------------
%%                       MISCELANEOUS
%%----------------------------------------------------------------------
-spec get_unixtime() -> integer(). %% now in sec
get_unixtime() ->
  {Mega, Secs, _Micro} = os:timestamp(),
  Timestamp = Mega * 1000000 + Secs,
  Timestamp.

%%  MISC
%%______________________________________________________________________________________________________________________
get_value(Key, List)->
  get_value(Key, List, undefined).
get_value(Key, List, Default)->
  case lists:keyfind(Key, 1, List) of
    {_, Val} -> Val;
    _        -> Default
  end.

%%  BINARIES
%%______________________________________________________________________________________________________________________
-spec bjoin(List :: list(binary())) -> binary().
bjoin([])  -> <<>>;
bjoin([H|T]) -> << H/bitstring, (bjoin(T))/bitstring >>.

bjoin([H|[]], _Sep) when is_binary(H) ->
  <<H/bitstring >>;
bjoin([H|T], Sep) when is_binary(H) ->
  << H/bitstring, Sep/bitstring, (bjoin(T, Sep))/bitstring >>.

config(Key, Config) when is_atom(Key) ->
  case get_value(Key, Config) of
    undefined ->
      throw({error, not_found});
    Value -> Value
  end.

-spec to_bin(binary()|list()|integer()|atom()|float()) -> binary().
to_bin(X) when is_binary(X) -> X;
to_bin(X) when is_list(X) -> list_to_binary(X);
to_bin(X) when is_integer(X) -> integer_to_binary(X);
to_bin(X) when is_atom(X) -> atom_to_binary(X, utf8);
to_bin(X) when is_float(X) -> float_to_binary(X, [{decimals, 4}]).


%%  HTTP_UTILS
%%______________________________________________________________________________________________________________________
join_form(Form) ->
  UrlPars  = [ << (urlencode(to_bin(K)))/binary, <<"=">>/binary, (urlencode(to_bin(V)))/binary >>||{K,V} <- Form ],
  bjoin(UrlPars, <<"&">>).

-spec urlencode(B) -> B when B::binary().
urlencode(B) ->
  urlencode(B, <<>>).

%%{{node[  ].hfwef}}

urlencode(<< $!, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $! >>);
urlencode(<< $$, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $$ >>);
urlencode(<< $&, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $& >>);
urlencode(<< $', Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $' >>);
urlencode(<< $(, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $( >>);
urlencode(<< $), Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $) >>);
urlencode(<< $*, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $* >>);
urlencode(<< $+, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $+ >>);
urlencode(<< $,, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $, >>);
urlencode(<< $-, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $- >>);
urlencode(<< $., Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $. >>);
urlencode(<< $0, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $0 >>);
urlencode(<< $1, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $1 >>);
urlencode(<< $2, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $2 >>);
urlencode(<< $3, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $3 >>);
urlencode(<< $4, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $4 >>);
urlencode(<< $5, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $5 >>);
urlencode(<< $6, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $6 >>);
urlencode(<< $7, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $7 >>);
urlencode(<< $8, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $8 >>);
urlencode(<< $9, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $9 >>);
urlencode(<< $:, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $: >>);
urlencode(<< $;, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $; >>);
urlencode(<< $=, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $= >>);
urlencode(<< $@, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $@ >>);
urlencode(<< $A, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $A >>);
urlencode(<< $B, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $B >>);
urlencode(<< $C, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $C >>);
urlencode(<< $D, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $D >>);
urlencode(<< $E, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $E >>);
urlencode(<< $F, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $F >>);
urlencode(<< $G, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $G >>);
urlencode(<< $H, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $H >>);
urlencode(<< $I, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $I >>);
urlencode(<< $J, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $J >>);
urlencode(<< $K, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $K >>);
urlencode(<< $L, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $L >>);
urlencode(<< $M, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $M >>);
urlencode(<< $N, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $N >>);
urlencode(<< $O, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $O >>);
urlencode(<< $P, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $P >>);
urlencode(<< $Q, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $Q >>);
urlencode(<< $R, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $R >>);
urlencode(<< $S, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $S >>);
urlencode(<< $T, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $T >>);
urlencode(<< $U, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $U >>);
urlencode(<< $V, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $V >>);
urlencode(<< $W, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $W >>);
urlencode(<< $X, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $X >>);
urlencode(<< $Y, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $Y >>);
urlencode(<< $Z, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $Z >>);
urlencode(<< $_, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $_ >>);
urlencode(<< $a, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $a >>);
urlencode(<< $b, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $b >>);
urlencode(<< $c, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $c >>);
urlencode(<< $d, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $d >>);
urlencode(<< $e, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $e >>);
urlencode(<< $f, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $f >>);
urlencode(<< $g, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $g >>);
urlencode(<< $h, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $h >>);
urlencode(<< $i, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $i >>);
urlencode(<< $j, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $j >>);
urlencode(<< $k, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $k >>);
urlencode(<< $l, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $l >>);
urlencode(<< $m, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $m >>);
urlencode(<< $n, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $n >>);
urlencode(<< $o, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $o >>);
urlencode(<< $p, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $p >>);
urlencode(<< $q, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $q >>);
urlencode(<< $r, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $r >>);
urlencode(<< $s, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $s >>);
urlencode(<< $t, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $t >>);
urlencode(<< $u, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $u >>);
urlencode(<< $v, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $v >>);
urlencode(<< $w, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $w >>);
urlencode(<< $x, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $x >>);
urlencode(<< $y, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $y >>);
urlencode(<< $z, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $z >>);
urlencode(<< $~, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $~ >>);
urlencode(<< C, Rest/bits >>, Acc) ->
  H = hex(C bsr 4),
  L = hex(C band 16#0f),
  urlencode(Rest, << Acc/bits, $%, H, L >>);
urlencode(<<>>, Acc) ->
  Acc.

hex( 0) -> $0;
hex( 1) -> $1;
hex( 2) -> $2;
hex( 3) -> $3;
hex( 4) -> $4;
hex( 5) -> $5;
hex( 6) -> $6;
hex( 7) -> $7;
hex( 8) -> $8;
hex( 9) -> $9;
hex(10) -> $A;
hex(11) -> $B;
hex(12) -> $C;
hex(13) -> $D;
hex(14) -> $E;
hex(15) -> $F.



-spec x_www_form_urlencoded(binary()) -> list({binary(), binary() | true}).
x_www_form_urlencoded(<<>>) ->
  [];
x_www_form_urlencoded(Qs) ->
  Tokens = binary:split(Qs, <<"&">>, [global, trim]),
  [case binary:split(Token, <<"=">>) of
     [Token] -> {urldecode(Token), true};
     [Name, Value] -> {urldecode(Name), urldecode(Value)}
   end || Token <- Tokens].


%% @doc Decode a URL encoded binary.
%% @equiv urldecode(Bin, crash)
-spec urldecode(binary()) -> binary().
urldecode(Bin) when is_binary(Bin) ->
  urldecode(Bin, <<>>, crash).

%% @doc Decode a URL encoded binary.
%% The second argument specifies how to handle percent characters that are not
%% followed by two valid hex characters. Use `skip' to ignore such errors,
%% if `crash' is used the function will fail with the reason `badarg'.
-spec urldecode(binary(), crash | skip) -> binary().
urldecode(Bin, OnError) when is_binary(Bin) ->
  urldecode(Bin, <<>>, OnError).

-spec urldecode(binary(), binary(), crash | skip) -> binary().
urldecode(<<$%, H, L, Rest/binary>>, Acc, OnError) ->
  G = unhex(H),
  M = unhex(L),
  if	G =:= error; M =:= error ->
    case OnError of skip -> ok; crash -> erlang:error(badarg) end,
    urldecode(<<H, L, Rest/binary>>, <<Acc/binary, $%>>, OnError);
    true ->
      urldecode(Rest, <<Acc/binary, (G bsl 4 bor M)>>, OnError)
  end;
urldecode(<<$%, Rest/binary>>, Acc, OnError) ->
  case OnError of skip -> ok; crash -> erlang:error(badarg) end,
  urldecode(Rest, <<Acc/binary, $%>>, OnError);
urldecode(<<$+, Rest/binary>>, Acc, OnError) ->
  urldecode(Rest, <<Acc/binary, $ >>, OnError);
urldecode(<<C, Rest/binary>>, Acc, OnError) ->
  urldecode(Rest, <<Acc/binary, C>>, OnError);
urldecode(<<>>, Acc, _OnError) ->
  Acc.

-spec unhex(byte()) -> byte() | error.
unhex(C) when C >= $0, C =< $9 -> C - $0;
unhex(C) when C >= $A, C =< $F -> C - $A + 10;
unhex(C) when C >= $a, C =< $f -> C - $a + 10;
unhex(_) -> error.
