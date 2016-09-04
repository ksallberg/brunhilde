-module(stats_server).

-author('kristian@purestyle.se').

-behaviour(gen_server).

-include("include/erlrest.hrl").

-export([start_link/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TIMEOUT, infinity).

-record(state, { connections :: integer()
               , server :: term()
               }).

-type state() :: #state{}.

start_link(Server) ->
    gen_server:start_link(?MODULE, Server, []).

init(Server = #{name := Name}) ->
    InitialState = #state{ server = Server
                         , connections = 0
                         },
    tracker_server:register(Name, self()),
    {ok, InitialState}.

handle_cast(inc_connections, #state{connections = Connections} = State) ->
    NewState = State#state{connections = Connections + 1},
    {noreply, NewState}.

-spec handle_call(any(), {pid(), any()}, state()) -> {stop, tuple(), state()}.
handle_call(get_stats, _From, State) ->
    {reply, State#state.connections, State}.

-spec handle_info(any(), state() | port()) -> {noreply, state()} |
                                              {stop, normal, state()} |
                                              {noreply, state(), infinity}.
handle_info(_Info, StateData) ->
    {noreply, StateData}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, _) ->
    ok.

%% For now, just return the received state data
-spec code_change(atom(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, StateData, _Extra) ->
    {ok, StateData}.
