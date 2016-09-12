-module(helloworld).

-export([ init/0
        , routes/0]).

-behaviour(rest_handler).

-include("include/erlrest.hrl").

-include_lib("xmerl/include/xmerl.hrl").

%% Called upon the start of the server, server
%% can do whatever it wishes do to here.
-spec init() -> atom().
init() ->
    ok.

routes() ->
    [ {json, get, "/helloworld/hello/", fun handle_hello/2}
    , {html, get, "/helloworld.html",   fun handle_html/2}
    , {html, get, "/helloworld2.html",  fun handle_html2/2}
    , {xml,  get, "/helloworld/xml/",   fun handle_xml/2}
    , {'*',                             fun handle_wildcard/2}].

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

handle_xml(_Data, _Parameters) ->
    [tree()].

%% tree is the property of:
%% https://gist.github.com/afternoon/5014291
tree() ->
    Ns1 = "http://cheese.com/names",
    Content = [{cheese, [{yarg, [{type, ["medium"]},
                                 #xmlElement{name=country,
                                             content=["nl"]}]}]}],
    #xmlElement{name=cheeses,
                namespace=#xmlNamespace{default=Ns1},
                attributes=[#xmlAttribute{name=xmlns, value=Ns1}],
                content=Content}.

handle_wildcard(_Data, _Parameters) ->
    <<"i dont know what youre saying">>.
