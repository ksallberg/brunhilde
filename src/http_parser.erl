%% Copyright (c) 2022, Kristian Sällberg
%%
% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% * Redistributions of source code must retain the above copyright notice, this
%%   list of conditions and the following disclaimer.
%%
%% * Redistributions in binary form must reproduce the above copyright notice,
%%   this list of conditions and the following disclaimer in the documentation
%%   and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
%% OF THE POSSIBILITY OF SUCH DAMAGE.

-module(http_parser).
-export([ parse_request/1
        , response/3
        , parameters/1
        , cookies/1
        , get_subdomain/1]).

-include_lib("eunit/include/eunit.hrl").

-type method() :: 'get' | 'head' | 'post' | 'put' |
                  'delete' | 'connect' | 'options' | 'trace' | 'patch'.
-type http_version() :: 'v10' | 'v11'.
-type http_info() :: {method(), string(), [{atom(), atom()}], http_version()}.
-type param() :: {binary(), binary()}.
-type header() :: param().
-type body() :: binary().

% Parse a request and return the
% request, headers and body as a tuple
-spec parse_request(string()) ->
                           {http_info(), [header()], body()} |
                           {atom(), term()}. %% error case
parse_request(R0) ->
    try
        {HttpInfo, R1}  = request_line(R0),
        {Headers, Body} = headers(R1),
        {HttpInfo, Headers, Body}
    catch Err:Why ->
            {Err, Why}
    end.

-spec request_line(binary()) -> {http_info(), binary()}.
request_line(<<"GET ", R0/binary>>) ->
    line_continue(get, R0);
request_line(<<"HEAD ", R0/binary>>) ->
    line_continue(head, R0);
request_line(<<"POST ", R0/binary>>) ->
    line_continue(post, R0);
request_line(<<"PUT ", R0/binary>>) ->
    line_continue(put, R0);
request_line(<<"DELETE ", R0/binary>>) ->
    line_continue(delete, R0);
request_line(<<"CONNECT ", R0/binary>>) ->
    line_continue(connect, R0);
request_line(<<"OPTIONS ", R0/binary>>) ->
    line_continue(options, R0);
request_line(<<"TRACE ", R0/binary>>) ->
    line_continue(trace, R0);
request_line(<<"PATCH ", R0/binary>>) ->
    line_continue(patch, R0).

-spec line_continue(method(), binary()) -> {http_info(), binary()}.
line_continue(Method, R0) ->
    {URI, R1} = request_uri(R0),
    {Ver, R2} = http_version(R1),
    <<13, 10, R3/binary>> = R2,
    [Route, Params] = case binary:split(URI, <<"?">>) of
                          [NoParams0, Params0] ->
                              [NoParams0, parameters(Params0)];
                          [NoParams0] ->
                              [NoParams0, []]
                      end,
    {{Method, Route, Params, Ver}, R3}.

% 32 is the last byte of the request uri (space), R0 is the last rest
-spec request_uri(binary()) -> {binary(), binary()}.
request_uri(<<" ",R0/binary>>) ->
    {<<"">>, R0};
% Until 32 is found, keep adding the head element
% to the request_uri return list
request_uri(<<C,R0/binary>>) ->
    {Rest, R1} = request_uri(R0),
    {<<C, Rest/binary>>, R1}.

-spec http_version(binary()) -> {atom(), binary()}.
http_version(<<"HTTP/1.1", R0/binary>>) ->
    {v11, R0};
http_version(<<"HTTP/1.0", R0/binary>>) ->
    {v10, R0}.

%% recursively pick out all headers until the stop CRLF
-spec headers(binary()) -> {[{binary(), binary()}], binary()}.
headers(<<"\r\n", R0/binary>>) ->
    {[],R0};
headers(R0) ->
    {Header0, R1} = header(R0),
    [Key, Value0] = binary:split(Header0, <<":">>),
    Value1 = case Value0 of
                 <<" ", ValueRest/binary>> ->
                     ValueRest;
                 _ ->
                     Value0
             end,
    {Rest, R2} = headers(R1),
    {[{Key, Value1}|Rest], R2}.

% take everything until [13,10]
% return that, and the rest of the list
% recursively pick out one header CRLF
-spec header(binary()) -> {binary(), binary()}.
header(<<"\r\n", R0/binary>>) ->
    {<<>>, R0};
header(<<C,R0/binary>>) ->
    {Rest, R1} = header(R0),
    {<<C,Rest/binary>>, R1}.

-spec parameters(binary()) -> [param()].
parameters(Ls) ->
    Params = binary:split(Ls, <<"&">>, [global]),
    lists:map(fun(Param) ->
                      [Key, Val] = binary:split(Param, <<"=">>),
                      {Key, Val}
              end, Params).

cookies(CookieString) ->
    Cs = binary:split(CookieString, <<"; ">>, [global]),
    lists:map(fun(KeyVal) ->
                      [A, B] = binary:split(KeyVal, <<"=">>),
                      {A, B}
              end, Cs).

get_subdomain(Headers) ->
    Host = proplists:get_value(<<"Host">>, Headers),
    case Host of
        undefined ->
            '*';
        _ ->
            case binary:split(Host, <<".">>, [global]) of
                [Subdomain, _Domain, _Rest] ->
                    Subdomain;
                _ ->
                    '*'
            end
    end.

-spec response(binary(), binary(), binary()) -> binary().
response(Body, ExtraHeaders, ReturnCode) ->
    Proto = <<"HTTP/1.1 ">>,
    CRLF = <<"\r\n">>,
    <<Proto/binary, ReturnCode/binary, CRLF/binary,
      ExtraHeaders/binary, CRLF/binary, Body/binary>>.

-ifdef(EUNIT).

parameters_test() ->
    ?assertEqual(http_parser:parameters(<<"username=apa&password=apa">>),
                 [{<<"username">>, <<"apa">>},
                  {<<"password">>, <<"apa">>}]).

post_test() ->
    Res =
        http_parser:parse_request(<<"POST /register_post HTTP/1.1\r\nHost: localhost:8000\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:102.0) Gecko/20100101 Firefox/102.0\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8\r\nAccept-Language: en-US,en;q=0.5\r\nAccept-Encoding: gzip, deflate, br\r\nContent-Type: application/x-www-form-urlencoded\r\nContent-Length: 25\r\nOrigin: http://localhost:8000\r\nConnection: keep-alive\r\nReferer: http://localhost:8000/register\r\nUpgrade-Insecure-Requests: 1\r\nSec-Fetch-Dest: document\r\nSec-Fetch-Mode: navigate\r\nSec-Fetch-Site: same-origin\r\nSec-Fetch-User: ?1\r\n\r\nusername=apa&password=apa">>),
    Expect = {{post,<<"/register_post">>,[],v11},
              [{<<"Host">>,<<"localhost:8000">>},
               {<<"User-Agent">>,
                <<"Mozilla/5.0 (X11; Linux x86_64; rv:102.0) Gecko/20100101 Firefox/102.0">>},
               {<<"Accept">>,
                <<"text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8">>},
               {<<"Accept-Language">>,<<"en-US,en;q=0.5">>},
               {<<"Accept-Encoding">>,<<"gzip, deflate, br">>},
               {<<"Content-Type">>,<<"application/x-www-form-urlencoded">>},
               {<<"Content-Length">>,<<"25">>},
               {<<"Origin">>,<<"http://localhost:8000">>},
               {<<"Connection">>,<<"keep-alive">>},
               {<<"Referer">>,<<"http://localhost:8000/register">>},
               {<<"Upgrade-Insecure-Requests">>,<<"1">>},
               {<<"Sec-Fetch-Dest">>,<<"document">>},
               {<<"Sec-Fetch-Mode">>,<<"navigate">>},
               {<<"Sec-Fetch-Site">>,<<"same-origin">>},
               {<<"Sec-Fetch-User">>,<<"?1">>}],
              <<"username=apa&password=apa">>},
    ?assertEqual(Res, Expect).

get_test() ->
    Res = http_parser:parse_request(<<"GET /?apa=mapa&fapa=dapa HTTP/1.1\r\nHost: localhost:5030\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:102.0) Gecko/20100101 Firefox/102.0\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8\r\nAccept-Language: en-US,en;q=0.5\r\nAccept-Encoding: gzip, deflate, br\r\nConnection: keep-alive\r\nCookie: username=5sCeEIvQggnFn3JjN0ua5SYvE9k%3D\r\nUpgrade-Insecure-Requests: 1\r\nSec-Fetch-Dest: document\r\nSec-Fetch-Mode: navigate\r\nSec-Fetch-Site: cross-site\r\n\r\n">>),
    Expect = {{get,<<"/">>,
               [{<<"apa">>,<<"mapa">>},{<<"fapa">>,<<"dapa">>}],
               v11},
              [{<<"Host">>,<<"localhost:5030">>},
               {<<"User-Agent">>,
                <<"Mozilla/5.0 (X11; Linux x86_64; rv:102.0) Gecko/20100101 Firefox/102.0">>},
               {<<"Accept">>,
                <<"text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8">>},
               {<<"Accept-Language">>,<<"en-US,en;q=0.5">>},
               {<<"Accept-Encoding">>,<<"gzip, deflate, br">>},
               {<<"Connection">>,<<"keep-alive">>},
               {<<"Cookie">>,<<"username=5sCeEIvQggnFn3JjN0ua5SYvE9k%3D">>},
               {<<"Upgrade-Insecure-Requests">>,<<"1">>},
               {<<"Sec-Fetch-Dest">>,<<"document">>},
               {<<"Sec-Fetch-Mode">>,<<"navigate">>},
               {<<"Sec-Fetch-Site">>,<<"cross-site">>}],
              <<>>},
    ?assertEqual(Res, Expect).

-endif.
