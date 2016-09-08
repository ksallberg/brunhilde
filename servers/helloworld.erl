-module(helloworld).

-export([ init/0
        , routes/0
        , wildcard/2]).

-behaviour(rest_handler).

-include("include/erlrest.hrl").

%% Called upon the start of the server, server
%% can do whatever it wishes do to here.
-spec init() -> atom().
init() ->
    ok.

routes() ->
    [ {json, get, "/helloworld/hello/", fun handle_hello/2}
    , {html, get, "/helloworld.html",   fun handle_html/2}
    , {html, get, "/helloworld2.html",  fun handle_html2/2}].

handle_hello(_Data, _Parameters) ->
    #{<<"hello">> => <<"hello2u">>}.

handle_html(_Data, _Parameters) ->
    Html = "<html>"
           "  <head>"
           "     <title>Hello!</title>"
           "  </head>"
           "  <body>"
           "    <h1>Hello world!!!</h1>"
           "    <a href='/helloworld2.html'>Go somewhere</a>"
           "  </body>"
           "</html>",
    ?l2b(Html).

handle_html2(_Data, _Parameters) ->
    Html = "<html>"
           "  <head>"
           "     <title>Hello!</title>"
           "  </head>"
           "  <body>"
           "    <h1>Hello world2!!!</h1>"
           "    <a href='/helloworld.html'>Go back</a>"
           "  </body>"
           "</html>",
    ?l2b(Html).

wildcard(_Data, _Parameters) ->
    <<"i dont know what youre saying">>.
