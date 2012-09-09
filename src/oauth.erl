-module(oauth).

-export([get/3, get/5, get/6, post/3, post/5, post/6, uri/2]).

get(Url, ExtraParams, Consumer) ->
    get(Url, ExtraParams, Consumer, "", "").

get(Url, ExtraParams, Consumer, Token, TokenSecret) ->
    get(Url, ExtraParams, Consumer, Token, TokenSecret, []).

get(Url, ExtraParams, Consumer, Token, TokenSecret, Options) ->
    SignedParams = sign("GET", Url, ExtraParams, Consumer, Token, TokenSecret),
    http_get(uri(Url, SignedParams), Options).

post(Url, ExtraParams, Consumer) ->
    post(Url, ExtraParams, Consumer, "", "").

post(Url, ExtraParams, Consumer, Token, TokenSecret) ->
    post(Url, ExtraParams, Consumer, Token, TokenSecret, []).

post(Url, ExtraParams, Consumer, Token, TokenSecret, Options) ->
    SignedParams = sign("POST", Url, ExtraParams, Consumer, Token, TokenSecret),
    http_post(Url, params_encode(SignedParams), Options).

uri(Base, []) ->
    Base;
uri(Base, Params) ->
    lists:concat([Base, "?", params_encode(Params)]).

consumer_key(_Consumer={Key, _, _}) ->
    Key.

consumer_secret(_Consumer={_, Secret, _}) ->
    Secret.

signature_method(_Consumer={_, _, Method}) ->
    Method.

sign(HttpMethod, Url, Params, Consumer, Token, TokenSecret) ->
    SignatureParams = signature_params(Consumer, Params, Token),
    Signature = signature(HttpMethod, Url, SignatureParams, Consumer, TokenSecret),
    [{"oauth_signature", Signature} | SignatureParams].

signature_params(Consumer, Params, "") ->
    signature_params(Consumer, Params);
signature_params(Consumer, Params, Token) ->
    signature_params(Consumer, [{"oauth_token", Token} | Params]).

signature_params(Consumer, Params) ->
    Timestamp = unix_timestamp(),
    Nonce = base64:encode_to_string(crypto:rand_bytes(32)), % cf. ruby-oauth
    [ {"oauth_version", "1.0"}
    , {"oauth_nonce", Nonce}
    , {"oauth_timestamp", integer_to_list(Timestamp)}
    , {"oauth_signature_method", signature_method_string(Consumer)}
    , {"oauth_consumer_key", consumer_key(Consumer)}
    | Params
    ].

signature(HttpMethod, Url, Params, Consumer, TokenSecret) ->
    case signature_method(Consumer) of
    plaintext ->
        plaintext_signature(Consumer, TokenSecret);
    hmac_sha1 ->
        hmac_sha1_signature(HttpMethod, Url, Params, Consumer, TokenSecret)
    end.

signature_method_string(Consumer) ->
    case signature_method(Consumer) of
    plaintext -> "PLAINTEXT";
    hmac_sha1 -> "HMAC-SHA1"
    end.

plaintext_signature(Consumer, TokenSecret) ->
    uri_join([consumer_secret(Consumer), TokenSecret]).

hmac_sha1_signature(HttpMethod, Url, Params, Consumer, TokenSecret) ->
    BaseString = signature_base_string(HttpMethod, Url, Params),
    hmac_sha1_signature(BaseString, Consumer, TokenSecret).

hmac_sha1_signature(BaseString, Consumer, TokenSecret) ->
    Key = uri_join([consumer_secret(Consumer), TokenSecret]),
    base64:encode_to_string(crypto:sha_mac(Key, BaseString)).

signature_base_string(HttpMethod, Url, Params) ->
    uri_join([HttpMethod, uri_normalize(Url), params_encode(Params)]).

params_encode(Params) ->
    % cf. http://tools.ietf.org/html/rfc5849#section-3.4.1.3.2
    Encoded = [{uri_encode(K), uri_encode(V)} || {K, V} <- Params],
    Sorted = lists:sort(Encoded),
    Concatenated = [lists:concat([K, "=", V]) || {K, V} <- Sorted],
    string:join(Concatenated, "&").

http_get(Url, Options) ->
    http_request(Url, [], get, [], Options).

http_post(Url, Data, Options) ->
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
    http_request(Url, Headers, post, Data, Options).

http_request(Url, Headers, Method, Data, Options) ->
    ibrowse:send_req(Url, Headers, Method, Data, Options).

unix_timestamp() ->
    unix_timestamp(calendar:universal_time()).

unix_timestamp(DateTime) ->
    unix_seconds(DateTime) - unix_epoch().

unix_epoch() ->
    unix_seconds({{1970,1,1}, {00,00,00}}).

unix_seconds(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime).

uri_normalize(Uri) ->
    case http_uri:parse(Uri) of
    {ok, {Scheme, UserInfo, Host, Port, Path, _Query}} ->
        uri_normalize(Scheme, UserInfo, string:to_lower(Host), Port, [Path]);
    Else ->
        Else
    end.

uri_normalize(http, UserInfo, Host, 80, Acc) ->
    uri_normalize(http, UserInfo, [Host | Acc]);
uri_normalize(https, UserInfo, Host, 443, Acc) ->
    uri_normalize(https, UserInfo, [Host | Acc]);
uri_normalize(Scheme, UserInfo, Host, Port, Acc) ->
    uri_normalize(Scheme, UserInfo, [Host, ":", Port|Acc]).

uri_normalize(Scheme, [], Acc) ->
    lists:concat([Scheme, "://" | Acc]);
uri_normalize(Scheme, UserInfo, Acc) ->
    lists:concat([Scheme, "://", UserInfo, "@" | Acc]).

uri_join(Values) ->
    uri_join(Values, "&").

uri_join(Values, Separator) ->
    string:join([uri_encode(Value) || Value <- Values], Separator).

uri_encode(Term) when is_integer(Term) ->
  integer_to_list(Term);
uri_encode(Term) when is_atom(Term) ->
  uri_encode(atom_to_list(Term));
uri_encode(Term) when is_list(Term) ->
  uri_encode(lists:reverse(Term, []), []).

-define(is_alphanum(C), C >= $A, C =< $Z; C >= $a, C =< $z; C >= $0, C =< $9).
uri_encode([X | T], Acc) when ?is_alphanum(X); X =:= $-; X =:= $_; X =:= $.; X =:= $~ ->
  uri_encode(T, [X | Acc]);
uri_encode([X | T], Acc) ->
  NewAcc = [$%, dec2hex(X bsr 4), dec2hex(X band 16#0f) | Acc],
  uri_encode(T, NewAcc);
uri_encode([], Acc) ->
  Acc.

-compile({inline, [{dec2hex, 1}]}).
dec2hex(N) when N >= 10 andalso N =< 15 ->
  N + $A - 10;
dec2hex(N) when N >= 0 andalso N =< 9 ->
  N + $0.
