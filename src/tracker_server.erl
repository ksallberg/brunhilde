%% Copyright (c) 2014-2016, Kristian Sällberg
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% * Redistributions of source code must retain the above copyright notice, this
%%   list of conditions and the following disclaimer.
%%
%% * Redistributions in binary form must reproduce the above copyright notice,
%%   this list of conditions and the following disclaimer in the documentation
%%   and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
%% OF THE POSSIBILITY OF SUCH DAMAGE.

-module(tracker_server).

-author('kristian@purestyle.se').

-behaviour(gen_server).

-include("include/erlrest.hrl").

-export([register/2,
         ask_for/1,
         get_stats/0]).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TIMEOUT, infinity).

-record(state, { stats_servers :: [term()] }).

-type state() :: #state{}.

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    case lists:member(?MODULE, erlang:registered()) of
        true ->
            erlang:unregister(?MODULE);
        false ->
            ok
    end,
    erlang:register(?MODULE, self()),
    InitialState = #state{ stats_servers = [] },
    {ok, InitialState}.

register(Name, Pid) ->
    gen_server:cast(?MODULE, {put, Name, Pid}).

ask_for(Name) ->
    gen_server:call(?MODULE, {get, Name}).

get_stats() ->
    Servers = gen_server:call(?MODULE, get_servers),
    [{Name, gen_server:call(Pid, get_stats)} || {Name, Pid} <- Servers].

handle_cast({put, ServerName, Pid},
            #state{stats_servers = StatsServers} = State) ->
    NewPair = {ServerName, Pid},
    NewStatsServers = lists:keystore(ServerName, 1, StatsServers, NewPair),
    {noreply, State#state{stats_servers = NewStatsServers}};
handle_cast(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout.~n", [self()]),
    {stop, normal, State}.

%% -spec handle_call(any(), {pid(), any()}, state()) -> term().
handle_call({get, ServerName},
            _From,
            #state{stats_servers = StatsServers} = State) ->
    {_Key, Value} = lists:keyfind(ServerName, 1, StatsServers),
    {reply, Value, State};
handle_call(get_servers, _From, #state{stats_servers = StatsServers} = State) ->
    {reply, StatsServers, State};
handle_call(Request, _From, State) ->
    {stop, {Request, undefined_event}, State}.

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
