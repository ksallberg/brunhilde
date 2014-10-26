-module(tcp_listener).
-author('kristian@purestyle.se').

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% FIXME: Change acceptor to port, and find out what
%%        prim_inet:async_accept returns
-record(state,
        {listener :: port(),   %% listening to socket
         acceptor :: any(), %% async acceptor's internal reference
         module   :: atom()    %% communication handling module
        }).

-type state()  :: #state{}.

-spec start_link(integer(), atom()) -> {ok, pid()} | ignore | {error, any()}.
start_link(Port, Module) when is_integer(Port), is_atom(Module) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, Module], []).

-spec init([integer() | atom()]) -> {ok, state()} | {stop, any()}.
init([Port, Module]) ->
    process_flag(trap_exit, true),
    Opts = [list, {packet, 0}, {reuseaddr, true}, {keepalive, true},
            {backlog, 30}, {active, false}],
    case gen_tcp:listen(Port, Opts) of
        {ok, Listen} ->
            {ok, Ref} = prim_inet:async_accept(Listen, -1),
            {ok, #state{listener = Listen,
                        acceptor = Ref,
                        module   = Module}};
        {error, Reason} ->
            {stop, Reason}
    end.

% here Request can be anything, we don't know...
-spec handle_call(any(), {pid(), any()}, state()) ->
    {stop, {atom(), any()}, state()}.
handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()} |
                                     {stop, any(), state()}.
handle_info({inet_async, ListSock, Ref, {ok, CliSock}},
            #state{listener=ListSock, acceptor=Ref, module=Module} = State) ->
    try
        case set_sockopt(ListSock, CliSock) of
            ok              -> ok;
            {error, Reason} -> exit({set_sockopt, Reason})
        end,

        {ok, Pid} = rest_server_app:start_client(),
        gen_tcp:controlling_process(CliSock, Pid),
        Module:set_socket(Pid, CliSock),

        case prim_inet:async_accept(ListSock, -1) of
            {ok,    NewRef} -> ok;
            {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,

        {noreply, State#state{acceptor=NewRef}}

    catch exit:Why ->
        error_logger:error_msg("Error in async accept ~p.~n", [Why]),
        {stop, Why, State}
    end;

handle_info({inet_async, _ListSock, _Ref, Error}, State) ->
    {stop, Error, State};

handle_info(_Info, State) ->
    {no_reply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, State) ->
    gen_tcp:close(State#state.listener),
    ok.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec set_sockopt(port(), port()) -> ok | any().
set_sockopt(ListSock, CliSock) ->
    true = inet_db:register_socket(CliSock, inet_tcp),
    SockSettings = [active, nodelay, keepalive, delay_send, priority, tos],
    case prim_inet:getopts(ListSock, SockSettings) of
        {ok, Opts} ->
            case prim_inet:setopts(CliSock, Opts) of
                ok    -> ok;
                Error -> gen_tcp:close(CliSock),
                         Error
            end;
        Error ->
            gen_tcp:close(CliSock),
            Error
    end.
