-module(tcp_server).
-author('kristian@purestyle.se').

-behaviour(gen_server).

-export([start_link/1,
         set_socket/2,
         init/1,
         handle_cast/2,
         handle_call/3,
         terminate/2,
         handle_info/2,
         code_change/3]).

-import(jiffy, [decode/1]).

% State while receiving bytes from the tcp socket
-record(state, { socket      :: port()                 %% client socket
               , addr        :: port()                 %% client address
               , data        :: string()               %% collected data
               , body_length :: integer()              %% total body length
               , route       :: string()               %% route expressed
                                                       %% as string()
               , parameters  :: [{string(), string()}] %% GET parameters
               , method      :: atom()                 %% method expressed as
                                                       %% atom(), get, post
               }).

-type state() :: #state{}.

-define(TIMEOUT, infinity).

-define(SOCK(Msg), {tcp, _Port, Msg}).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    %% properly seeding the process
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    %% Because accepting a connection is a blocking function call,
    %% we can not do it in here. Forward to the server loop!
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket, data="", body_length=-1, route=unknown}}.

-spec set_socket(pid(), port()) -> ok. %% cast = ok
set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_server:cast(Pid, {socket_ready, Socket}).

%% No request JSON given...
respond(#state{socket = S, data = [], route = Route,
               method = Method, parameters = Parameters}) ->
    Answer     = route_handler:match(Method, Route, no_json, Parameters),
    JsonReturn = jiffy:encode(Answer),
    ok         = gen_tcp:send(S, http_parser:response(JsonReturn)),
    gen_tcp:close(S);

%% Request JSON given...
respond(#state{socket = S, data = Body, route = Route,
               method = Method, parameters = Parameters}) ->
    JsonObj    = jiffy:decode(Body),
    Answer     = route_handler:match(Method, Route, JsonObj, Parameters),
    JsonReturn = jiffy:encode(Answer),
    ok         = gen_tcp:send(S, http_parser:response(JsonReturn)),
    gen_tcp:close(S).

-spec handle_cast({data, string()} | timeout | {socket_ready, port()}, state())
    -> {stop, normal, state()} | {noreply, state(), infinity}.
handle_cast(accept, S = #state{socket=ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    %% Remember that thou art dust, and to dust thou shalt return.
    %% We want to always keep a given number of children in this app.
    tcp_supervisor:start_socket(), % a new acceptor is born, praise the lord
    {noreply, S#state{socket=AcceptSocket}};

%% Handle the actual client connecting and requesting something (finished)
handle_cast({data, Data}, #state{data = DBuf, body_length = BL} = State) ->
    case length(Data ++ DBuf) == BL of
        true ->
            NewState = State#state{data = DBuf ++ Data},
            respond(NewState),
            {stop, normal, NewState};
        false ->
            NewState = case BL of
                -1 ->
                    {{Method, Route, Params, v11}, Headers, Body}
                        = http_parser:parse_request(Data),
                    NewBL     = get_content_length(Headers),
                    NewRoute  = Route,
                    State#state{data        = DBuf ++ Body,
                                body_length = NewBL,
                                route       = NewRoute,
                                parameters  = Params,
                                method      = Method};
                _ ->
                    State#state{data=DBuf ++ Data}
            end,
            case length(NewState#state.data) == NewState#state.body_length of
                true  ->
                    respond(NewState),
                    {stop, normal, NewState};
                false ->
                    {noreply, NewState, ?TIMEOUT}
            end
    end;

handle_cast(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout.~n", [self()]),
    {stop, normal, State}.

-spec handle_call(any(), {pid(), any()}, state()) -> {stop, tuple(), state()}.
handle_call(Request, _From, State) ->
    {stop, {Request, undefined_event}, State}.

-spec handle_info(any(), state() | port()) -> {noreply, state()} |
                                              {stop, normal, state()} |
                                              {noreply, state(), infinity}.
handle_info({tcp, Sock, Bin}, #state{socket=Sock} = StateData) ->
    inet:setopts(Sock, [{active, once}]),
    ?MODULE:handle_cast({data, Bin}, StateData);

handle_info({tcp_closed, Socket},
            #state{socket=Socket, addr=Addr} = StateData) ->
    error_logger:info_msg("~p Client ~p disconnected. ~n", [self(), Addr]),
    {stop, normal, StateData};

handle_info(_Info, StateData) ->
    {noreply, StateData}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, #state{socket=Socket}) ->
    (catch gen_tcp:close(Socket)),
    ok.

%% For now, just return the received state data
-spec code_change(atom(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, StateData, _Extra) ->
    {ok, StateData}.

-spec get_content_length([string()]) -> integer().
get_content_length(Headers) ->
    case [Len || [Desc, Len] <- [string:tokens(Header, " ")
              || Header <- Headers], Desc=="Content-Length:"] of
        []     -> 0;
        ConLen -> A = lists:nth(1, ConLen),
                      {Num, _} = string:to_integer(A),
                      Num
    end.
