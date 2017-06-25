-module(helloworld).

-export([ init/1
        , routes/0]).

-behaviour(http_handler).

-include("./include/brunhilde.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% Called upon the start of the server, server
%% can do whatever it wishes do to here.
-spec init(atom()) -> atom().
init(InstanceName) ->
    ?liof("I have been initialized, I am ~p~n", [InstanceName]),
    ok.

routes() ->
    [ #route{protocol = json,
             verb = get,
             address = "/helloworld/hello/",
             callback = fun handle_hello/4}
    , #route{protocol = json,
             verb = get,
             address = "/error",
             callback = fun handle_error/4}
    , #route{protocol = html,
             verb = get,
             address = "/helloworld.html",
             callback = fun handle_html/4}
    , #route{protocol = html,
             verb = get,
             address = "/helloworld2.html",
             callback = fun handle_html2/4}
    , #route{protocol = xml,
             verb = get,
             address = "/helloworld/xml/",
             callback = fun handle_xml/4}
    , #route{protocol = file,
             verb = get,
             address = "/helloworld/brunhilde.jpg",
             callback = fun handle_pic/4}
    , #route{protocol = html,
             verb = get,
             address = "/helloworld/template",
             callback = fun handle_template/4}
    , #route{protocol = html,
             verb = get,
             address = "/setcookie",
             callback = fun set_cookie/4}
    , #route{protocol = html,
             verb = get,
             address = "/hascookie",
             callback = fun has_cookie/4}
    , #route{protocol = file,
             verb = get,
             address = "/favicon.ico",
             callback = fun handle_icon/4}
    , {'*', fun handle_wildcard/4}].

handle_hello(_Data, Parameters, _Headers, _InstanceName) ->
    lager:log(info, self(), "Someone asked for ~p", [Parameters]),
    #{<<"hello">> => <<"hello2u">>}.

handle_error(_Data, _Parameters, _Headers, _InstanceName) ->
    lager:log(error, self(), "some error!"),
    #{<<"hello">> => <<"error">>}.

handle_html(_Data, _Parameters, _Headers, _InstanceName) ->
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

handle_html2(_Data, _Parameters, _Headers, _InstanceName) ->
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

handle_xml(_Data, _Parameters, _Headers, _InstanceName) ->
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

handle_template(_, _, _Headers, _InstanceName) ->
    {ok, Module} = erlydtl:compile_file("static/example_template.dtl",
                                        template_name),
    {ok, Binary} = Module:render([ {thursday, <<"this is a day">>}
                                 , {day, true}
                                 ]),
    Binary.

handle_pic(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("static/brunhilde.jpg"),
    Binary.

handle_icon(_, _, _, _InstanceName) ->
    {ok, Binary} = file:read_file("static/favicon.ico"),
    Binary.

set_cookie(_Data, _Parameters, _Headers, _InstanceName) ->
    #{response      => <<"Cookie set!">>,
      extra_headers => "Set-Cookie: theme=dark\r\n"
                       "Set-Cookie: user=alice\r\n"}.

has_cookie(_Data, _Parameters, Headers, _InstanceName) ->
    Cookies = [Cookies || {"Cookie", Cookies} <- Headers],
    Return = case Cookies of
                 [] ->
                     <<"No cookie set.">>;
                 _ ->
                     ?l2b("Following cookies are set: " ++ Cookies)
             end,
    Return.

handle_wildcard(_Data, _Parameters, _Headers, _InstanceName) ->
    <<"Error 404: i dont know what youre saying">>.
