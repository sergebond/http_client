-author("srg").

-define(DELAY, 500).
-define(MAX_ATTEMPTS, 3).

-type content_type() :: string().
%% "application/json"| "application/xml" | "multipart/form-data" | "application/x-www-form-urlencoded"
%% |"text/xml"|"text/json"|"text|csv"

-type method() :: get|post|put|delete|options.

-record(http_request_profile, {
  method = post :: method(),
  url :: string(),
  content_type = "text/plain" :: content_type(),
  charset = "charset=utf-8" :: string(),
  headers = [] :: proplists:proplist()|[],
  http_options = [] :: proplists:proplist()|[], %%{timeout, timer:seconds(5)}
  options = [{body_format, binary}] :: proplists:proplist()|[],
  attempts = ?MAX_ATTEMPTS,
  delay = ?DELAY,
  resp_converter = auto :: json | xml | 'x-form' | multipart | none | auto %% @todo
}).

-record(http_response, {
  status :: integer(),
  head :: proplists:proplist(),
  body :: term()
}).