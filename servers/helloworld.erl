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
    [ {json, get, "/helloworld/hello/",        fun handle_hello/2}
    , {json, get, "/error",                    fun handle_error/2}
    , {html, get, "/helloworld.html",          fun handle_html/2}
    , {html, get, "/helloworld2.html",         fun handle_html2/2}
    , {xml,  get, "/helloworld/xml/",          fun handle_xml/2}
    , {file, get, "/helloworld/brunhilde.jpg", fun handle_pic/2}
    , {html, get, "/helloworld/template",      fun handle_template/2}
    , {'*',                                    fun handle_wildcard/2}].

handle_hello(_Data, Parameters) ->
    lager:log(info, self(), "Someone asked for ~p", [Parameters]),
    #{<<"hello">> => <<"hello2u">>}.

handle_error(_Data, _Parameters) ->
    lager:log(error, self(), "some error!"),
    #{<<"hello">> => <<"error">>}.

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
           "    <img src='/helloworld/brunhilde.jpg'></img>"
           "    <br/>"
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

handle_template(_, _) ->
    {ok, Module} = erlydtl:compile_file("static/example_template.dtl",
                                        template_name),
    {ok, Binary} = Module:render([ {thursday, true}
                                 , {day, true}
                                 ]),
    Binary.

handle_pic(_, _) ->
    {ok, Binary} = file:read_file("static/brunhilde.jpg"),
    Binary.

handle_wildcard(_Data, _Parameters) ->
    <<"Error 404: i dont know what youre saying">>.
