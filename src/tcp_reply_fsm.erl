-module(tcp_reply_fsm).
-author('kristian@purestyle.se').

-behaviour(gen_fsm).

-export([start_link/0, set_socket/2]).

-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-export([wait_for_socket/2, wait_for_data/2]).

-import(jiffy, [decode/1]).

-record(state, { socket, %% client socket
                 addr    %% client address
               }).

-define(TIMEOUT, infinity).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

init([]) ->
    process_flag(trap_exit, true),
    {ok, wait_for_socket, #state{}}.

wait_for_socket({socket_ready, Socket}, State) when is_port(Socket) ->
    inet:setopts(Socket, [list, {active, once}]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    {next_state, wait_for_data, State#state{socket=Socket, addr=IP}, ?TIMEOUT};

wait_for_socket(Other, State) ->
    error_logger:error_msg("State: wait_for_socket. Unexpected: ~p~n",
                           [Other]),
    {next_state, wait_for_socket, State}.

%% Handle the actual client connecting and requesting something
wait_for_data({data, Data}, #state{socket=S} = State) ->
    {{get, Route, v11}, _Headers, Body} = http_parser:parse_request(Data),
    JsonObj    = jiffy:decode(Body),
    Answer     = route_handler:match(Route, JsonObj),
    JsonReturn = jiffy:encode(Answer),
    ok         = gen_tcp:send(S, http_parser:response(JsonReturn)),
    gen_tcp:close(S),
    {stop, normal, State};

wait_for_data(timeout, State) ->
    io:format("timeout...~n"),
    error_logger:error_msg("~p Client connection timeout.~n", [self()]),
    {stop, normal, State};

wait_for_data(Data, State) ->
    io:format("~p Ignoring data: ~p~n", [self(), Data]),
    {next_state, wait_for_data, State, ?TIMEOUT}.

handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

handle_info({tcp, Sock, Bin}, StateName, #state{socket=Sock} = StateData) ->
    inet:setopts(Sock, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName,
            #state{socket=Socket, addr=Addr} = StateData) ->
    error_logger:info_msg("~p Client ~p disconnected. ~n", [self(), Addr]),
    {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.

terminate(_Reason, _StateName, #state{socket=Socket}) ->
    (catch gen_tcp:close(Socket)),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.