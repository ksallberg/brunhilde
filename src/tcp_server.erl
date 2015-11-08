-module(tcp_server).

-export([init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         handle_info/2,
         start_link/1
       ]).

-behavior(gen_server).

start_link(undefined) ->
    io:format("error!~n");

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(_Socket) ->
    %% properly seeding the process
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    %% Because accepting a connection is a blocking function call,
    %% we can not do it in here. Forward to the server loop!
    gen_server:cast(self(), accept),
    {ok, hej}.

code_change(_, _, _) ->
    {ok, hej}.

handle_call(_, _, _) ->
    {ok, hej}.

handle_cast(_, _) ->
    {ok, hej}.

terminate(_, _) ->
    {ok, hej}.

handle_info(_, _) ->
    {ok, hej}.

%% %% No request JSON given...
%% -spec respond(state()) -> ok.
%% respond(#state{socket = S, data = [], route = Route,
%%                method = Method, parameters = Parameters}) ->
%%     Answer     = route_handler:match(Method, Route, no_json, Parameters),
%%     JsonReturn = jiffy:encode(Answer),
%%     ok         = gen_tcp:send(S, http_parser:response(JsonReturn)),
%%     gen_tcp:close(S);

%% %% Request JSON given...
%% respond(#state{socket = S, data = Body, route = Route,
%%                method = Method, parameters = Parameters}) ->
%%     JsonObj    = jiffy:decode(Body),
%%     Answer     = route_handler:match(Method, Route, JsonObj, Parameters),
%%     JsonReturn = jiffy:encode(Answer),
%%     ok         = gen_tcp:send(S, http_parser:response(JsonReturn)),
%%     gen_tcp:close(S).

%% %% Handle the actual client connecting and requesting something (finished)
%% -spec handle_cast({data, string()} | timeout | {socket_ready, port()}, state())
%%     -> {stop, normal, state()} | {noreply, state(), infinity}.
%% handle_cast({data, Data}, #state{data = DBuf, body_length = BL} = State) ->
%%     case length(Data ++ DBuf) == BL of
%%         true ->
%%             NewState = State#state{data = DBuf ++ Data},
%%             respond(NewState),
%%             {stop, normal, NewState};
%%         false ->
%%             NewState = case BL of
%%                 -1 ->
%%                     {{Method, Route, Params, v11}, Headers, Body}
%%                         = http_parser:parse_request(Data),
%%                     NewBL     = get_content_length(Headers),
%%                     NewRoute  = Route,
%%                     State#state{data        = DBuf ++ Body,
%%                                 body_length = NewBL,
%%                                 route       = NewRoute,
%%                                 parameters  = Params,
%%                                 method      = Method};
%%                 _ ->
%%                     State#state{data=DBuf ++ Data}
%%             end,
%%             case length(NewState#state.data) == NewState#state.body_length of
%%                 true  ->
%%                     respond(NewState),
%%                     {stop, normal, NewState};
%%                 false ->
%%                     {noreply, NewState, ?TIMEOUT}
%%             end
%%     end;
