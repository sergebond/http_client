# HTTP_CLIENT
## basic usage
```
  Url = ?TEST_URL ++ "/post",
  Profile =
    #http_request_profile{
      url = Url,
      method = post,
      content_type = "application/json"
    },

  ReqBody = [{<<"aoo">>, <<"q">>}, {<<"bar">>, <<"w">>}, {<<"baz">>, <<"w">>}],

  #http_response{status = 200, head = Head, body = RespBody } = http_client:request(ReqBody, Profile),
  true = is_list(RespBody)
```