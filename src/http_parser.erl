-module(http_parser).
-export([parse_request/1, response/1]).

% Parse a request and return the
% request, headers and body as a tuple
parse_request(R0) ->
    {Request, R1} = request_line(R0),
    {Headers, R2} = headers(R1),
    {Body,    _ } = message_body(R2),
    {Request, Headers, Body}.

request_line([$G, $E, $T, 32 | R0]) ->
    line_continue(get, R0);

request_line([$P, $U, $T, 32 | R0]) ->
    line_continue(put, R0);

request_line([$P, $O, $S, $T, 32 | R0]) ->
    line_continue(post, R0);

request_line([$D, $E, $L, $E, $T, $E, 32 | R0]) ->
    line_continue(delete, R0).

throw_params(URI) ->
    lists:takewhile(fun(X) -> X /= $? end, URI).

keep_params(URI) ->
    case lists:dropwhile(fun(X) -> X /= $? end, URI) of
        [] -> []; %no GET parameters encountered
        Ls -> parameters(lists:nthtail(1, Ls)) % first, remove the ? char
    end.

line_continue(Method, R0) ->
    {URI, R1}     = request_uri(R0),
    {Ver, R2}     = http_version(R1),
    [13, 10 | R3] = R2,
    {{Method, throw_params(URI), keep_params(URI), Ver}, R3}.

% 32 is the last byte of the request uri (space), R0 is the last rest
request_uri([32|R0]) ->
    {[], R0};
% intil 32 is found, keep adding the head element
% to the request_uri return list
request_uri([C|R0]) ->
    {Rest, R1} = request_uri(R0),
    {[C | Rest], R1}.

http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
    {v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
    {v10, R0}.

% recursively pick out all headers until
% the stop "[13,10]" CRLF
headers([13, 10 | R0]) ->
    {[],R0};
headers(R0) ->
    {Header, R1} = header(R0),
    {Rest,   R2} = headers(R1),
    {[Header|Rest], R2}.

% take everything until [13,10]
% return that, and the rest of the list
% recursively pick out one header CRLF
header([13, 10 | R0]) ->
    {[], R0};
header([C|R0]) ->
    {Rest, R1} = header(R0),
    {[C|Rest], R1}.

message_body(R) ->
    {R, []}.

parameters(Ls) ->
    {Parameter, Rest} = parameter(Ls),
    case Rest of
        [] -> [Parameter];
        _  -> [Parameter] ++ parameters(lists:nthtail(1,Rest))
    end.

parameter(Ls) ->
    {Name, Rest}     = lists:splitwith(fun(X) -> X /= $= end, Ls),
    {Content, Rest2} = lists:splitwith(fun(X) -> X /= $& end,
                                       lists:nthtail(1,Rest)
                                      ), % drop = char
    {{Name, Content}, Rest2}.

%% for now always send access-control-allow
response(Body) ->
    "HTTP/1.1 200 OK\r\n" ++
    "Access-Control-Allow-Origin: *\r\n"++ "\r\n" ++ Body.
