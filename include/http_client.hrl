-author("srg").

-type content_type() :: string().
%% "application/json"| "application/xml" | "multipart/form-data" | "application/x-www-form-urlencoded"
%% |"text/xml"|"text/json"|"text|csv"

-type method() :: get|post|put|delete|options.

-record(http_client_profile, {
  method = post :: method(),
  url :: string(),
  content_type = [] :: content_type(),
  headers = [] :: list(),
  http_options = [], %%{timeout, timer:seconds(5)}
  options = [{body_format, binary}] :: list(),
  resp_converter = none
}).