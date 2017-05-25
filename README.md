# HTTP_CLIENT
Makes http requests with serializing request body according to profile and deserializing response body according to content type

Supports json, xml, form_urlencoded

todo multipart

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
... to be continued

You can find more use cases in tests folder