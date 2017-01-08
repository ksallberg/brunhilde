-module(helloworld).

-export([ init/1
        , routes/0]).

-behaviour(http_handler).

-include("include/brunhilde.hrl").

-include_lib("xmerl/include/xmerl.hrl").

%% Called upon the start of the server, server
%% can do whatever it wishes do to here.
-spec init(atom()) -> atom().
init(InstanceName) ->
    ?liof("I have been initialized, I am ~p~n", [InstanceName]),
    ok.

routes() ->
    [ {json, get, "/helloworld/hello/",        fun handle_hello/3}
    , {json, get, "/error",                    fun handle_error/3}
    , {html, get, "/helloworld.html",          fun handle_html/3}
    , {html, get, "/helloworld2.html",         fun handle_html2/3}
    , {xml,  get, "/helloworld/xml/",          fun handle_xml/3}
    , {file, get, "/helloworld/brunhilde.jpg", fun handle_pic/3}
    , {html, get, "/helloworld/template",      fun handle_template/3}
    , {html, get, "/setcookie",                fun set_cookie/3}
    , {html, get, "/hascookie",                fun has_cookie/3}
    , {file, get, "/favicon.ico",              fun handle_icon/3}
    , {'*',                                    fun handle_wildcard/3}].

handle_hello(_Data, Parameters, _Headers) ->
    lager:log(info, self(), "Someone asked for ~p", [Parameters]),
    #{<<"hello">> => <<"hello2u">>}.

handle_error(_Data, _Parameters, _Headers) ->
    lager:log(error, self(), "some error!"),
    #{<<"hello">> => <<"error">>}.

handle_html(_Data, _Parameters, _Headers) ->
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

handle_html2(_Data, _Parameters, _Headers) ->
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

handle_xml(_Data, _Parameters, _Headers) ->
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

handle_template(_, _, _Headers) ->
    {ok, Module} = erlydtl:compile_file("static/example_template.dtl",
                                        template_name),
    {ok, Binary} = Module:render([ {thursday, <<"this is a day">>}
                                 , {day, true}
                                 ]),
    Binary.

handle_pic(_, _, _) ->
    {ok, Binary} = file:read_file("static/brunhilde.jpg"),
    Binary.

handle_icon(_, _, _) ->
    {ok, Binary} = file:read_file("static/favicon.ico"),
    Binary.

set_cookie(_Data, _Parameters, _Headers) ->
    #{response      => <<"Cookie set!">>,
      extra_headers => "Set-Cookie: theme=dark\r\n"
                       "Set-Cookie: user=alice\r\n"}.

has_cookie(_Data, _Parameters, Headers) ->
    Cookies = [Cookies || {"Cookie", Cookies} <- Headers],
    Return = case Cookies of
                 [] ->
                     <<"No cookie set.">>;
                 _ ->
                     ?l2b("Following cookies are set: " ++ Cookies)
             end,
    Return.

handle_wildcard(_Data, _Parameters, _Headers) ->
    <<"Error 404: i dont know what youre saying">>.
