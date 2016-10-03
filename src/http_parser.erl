%% Copyright (c) 2014-2016, Kristian Sällberg
%% All rights reserved.
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
        , cookies/1]).

-type method() :: 'delete' | 'get' | 'post' | 'put'.
-type http_version() :: 'v10' | 'v11'.
-type http_info() :: {method(), string(), [{atom(), atom()}], http_version()}.
-type param() :: {string(), string()}.

% Parse a request and return the
% request, headers and body as a tuple
-spec parse_request(string()) -> {http_info(), [string()], string()}.
parse_request(R0) ->
    {HttpInfo, R1}  = request_line(R0),
    {Headers, Body} = headers(R1),
    {HttpInfo, Headers, Body}.

-spec request_line(string()) -> {http_info(), string()}.
request_line([$G, $E, $T, 32 | R0]) ->
    line_continue(get, R0);
request_line([$P, $U, $T, 32 | R0]) ->
    line_continue(put, R0);
request_line([$P, $O, $S, $T, 32 | R0]) ->
    line_continue(post, R0);
request_line([$D, $E, $L, $E, $T, $E, 32 | R0]) ->
    line_continue(delete, R0).

-spec throw_params(string()) -> string().
throw_params(URI) ->
    lists:takewhile(fun(X) -> X /= $? end, URI).

-spec keep_params(string()) -> [param()].
keep_params(URI) ->
    case lists:dropwhile(fun(X) -> X /= $? end, URI) of
        [] -> []; %no GET parameters encountered
        Ls -> parameters(lists:nthtail(1, Ls)) % first, remove the ? char
    end.

-spec line_continue(atom(), string()) -> {http_info(), string()}.
line_continue(Method, R0) ->
    {URI, R1}     = request_uri(R0),
    {Ver, R2}     = http_version(R1),
    [13, 10 | R3] = R2,
    {{Method, throw_params(URI), keep_params(URI), Ver}, R3}.

% 32 is the last byte of the request uri (space), R0 is the last rest
-spec request_uri(string()) -> {string(), string()}.
request_uri([32|R0]) ->
    {[], R0};
% intil 32 is found, keep adding the head element
% to the request_uri return list
request_uri([C|R0]) ->
    {Rest, R1} = request_uri(R0),
    {[C | Rest], R1}.

-spec http_version(string()) -> {atom(), string()}.
http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
    {v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
    {v10, R0}.

% recursively pick out all headers until
% the stop "[13,10]" CRLF
-spec headers(string()) -> {[{string(), string()}], string()}.

headers([13, 10 | R0]) ->
    {[],R0};
headers([]) ->
    {[], []};
headers(R0) ->
    {Header0, R1} = header(R0),
    NotColon = fun(Input) -> Input /= $: end,
    HeaderKey   = lists:takewhile(NotColon, Header0),
    %% Drop "xxxx: "
    HeaderValue = tl(tl(lists:dropwhile(NotColon, Header0))),
    {Rest,   R2} = headers(R1),
    {[{HeaderKey, HeaderValue}|Rest], R2}.

% take everything until [13,10]
% return that, and the rest of the list
% recursively pick out one header CRLF
-spec header(string()) -> {string(), string()}.
header([13, 10 | R0]) ->
    {[], R0};
header([C|R0]) ->
    {Rest, R1} = header(R0),
    {[C|Rest], R1}.

-spec parameters(string()) -> [param()].
parameters(Ls) ->
    {Parameter, Rest} = parameter(Ls),
    case Rest of
        [] -> [Parameter];
        _  -> [Parameter] ++ parameters(lists:nthtail(1,Rest))
    end.

-spec parameter(string()) -> {param(), string()}.
parameter(Ls) ->
    {Name, Rest}     = lists:splitwith(fun(X) -> X /= $= end, Ls),
    {Content, Rest2} = lists:splitwith(fun(X) -> X /= $& end,
                                       lists:nthtail(1,Rest)
                                      ), % drop = char
    {{Name, Content}, Rest2}.

cookies(CookieString) ->
    Cs = re:split(CookieString, "; ", [{return, list}]),
    lists:map(fun(X) ->
                      [A, B] = re:split(X, "=", [{return, list}]),
                      {A, B}
              end, Cs).

%% for now always send access-control-allow
-spec response(string(), string(), string()) -> string().
response(Body, ExtraHeaders, ReturnCode) ->
    "HTTP/1.1 " ++ ReturnCode ++ "\r\n" ++
    "Access-Control-Allow-Origin: *\r\n" ++
    ExtraHeaders ++ "\r\n" ++ Body.
