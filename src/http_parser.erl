-module(http_parser).
-export([parse_request/1, response/1]).

-type method() :: 'delete' | 'get' | 'post' | 'put'.
-type http_version() :: 'v10' | 'v11'.
-type http_info() :: {method(), string(), [{atom(), atom()}], atom()}.
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
-spec headers(string()) -> {[string()], string()}.
headers([13, 10 | R0]) ->
    {[],R0};
headers(R0) ->
    {Header, R1} = header(R0),
    {Rest,   R2} = headers(R1),
    {[Header|Rest], R2}.

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

%% for now always send access-control-allow
-spec response(string()) -> string().
response(Body) ->
    "HTTP/1.1 200 OK\r\n" ++
    "Access-Control-Allow-Origin: *\r\n"++ "\r\n" ++ Body.
