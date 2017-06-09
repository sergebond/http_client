-author("srg").

-export_type([
  profile/0,
  response/0]).

-define(DELAY, 500).
-define(DEFAULT_MAX_ATTEMPTS, 1).

-type content_type() :: string().
%% "application/json"| "application/xml" | "multipart/form-data" | "application/x-www-form-urlencoded"
%% |"text/xml"|"text/json"|"text|csv"

-type method() :: get|post|put|delete|options.
-type format() :: json | xml | 'x-form' | multipart | none.%% @todo

-record(http_request_profile, {
  method = post :: method(),
  url :: string(),
  content_type = "text/plain" :: content_type(),
  charset = "charset=utf-8" :: string(),
  headers = [] :: proplists:proplist()|[],
  http_options = [{autoredirect, false}] :: proplists:proplist()|[], %%{timeout, timer:seconds(5)}
  options = [{body_format, binary}] :: proplists:proplist()|[],
  attempts = ?DEFAULT_MAX_ATTEMPTS,
  delay = ?DELAY,
  resp_converter = auto :: format() | auto
}).

-record(http_response, {
  status :: integer(),
  head :: proplists:proplist(),
  body :: term()
}).

-type profile() :: #http_request_profile{}.
-type response() :: #http_response{}.