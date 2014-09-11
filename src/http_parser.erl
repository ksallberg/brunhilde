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
    {URI, R1}     = request_uri(R0),
    {Ver, R2}     = http_version(R1),
    [13, 10 | R3] = R2,
    {{get, URI, Ver}, R3}.

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

response(Body) ->
    "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.
