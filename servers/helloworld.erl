-module(helloworld).

-export([init/1, routes/0]).

-behaviour(http_handler).

-include("./include/brunhilde.hrl").

%% Called upon the start of the server, server
%% can do whatever it wishes do to here.
-spec init(atom()) -> atom().
init(InstanceName) ->
    ?liof("I have been initialized, I am ~p~n", [InstanceName]),
    ok.

routes() ->
    [#route{verb = get,
            address = <<"/helloworld.html">>,
            callback = fun handle_html/4},

     #route{verb = post,
            address = <<"/handle_post">>,
            callback = fun handle_post/4},

     {'*', fun handle_wildcard/4}
    ].

handle_html(_Data, _Parameters, _Headers, _InstanceName) ->
    Html = "<html>"
           "  <head>"
           "     <title>Hello!</title>"
           "  </head>"
           "  <body>"
           "    <h1>Hello world!!!</h1>"
           "  </body>"
           "</html>",
    ?l2b(Html).

handle_post(_Data, _Parameters, _Headers, _InstanceName) ->
    Html = "<html>"
           "  <head>"
           "     <title>Hello!</title>"
           "  </head>"
           "  <body>"
           "    <h1>Hello world!!!</h1>"
           "  </body>"
           "</html>",
    ?l2b(Html).

handle_wildcard(_Data, _Parameters, _Headers, _InstanceName) ->
    <<"Error 404: i dont know what youre saying">>.
