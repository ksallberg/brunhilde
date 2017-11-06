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

-module(reloader_server).

-author('kristian@purestyle.se').

-behaviour(gen_server).

-include("brunhilde.hrl").

-export([start_link/2]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TIMEOUT, 60000).

start_link(Servers, Flags) ->
    gen_server:start_link(?MODULE, [Servers, Flags], []).

init([Servers, Flags]) ->
    case lists:member(?MODULE, erlang:registered()) of
        true ->
            erlang:unregister(?MODULE);
        false ->
            ok
    end,
    erlang:register(?MODULE, self()),
    InitialState = Servers,
    case ?flag_set(?USE_RELOADER, Flags) of
        false ->
            ok;
        true ->
            erlang:send_after(?TIMEOUT, self(), trigger)
    end,
    {ok, InitialState}.

handle_cast(_, State) ->
    {noreply, State}.

handle_call(_, _From, State) ->
    {noreply, State}.

handle_info(trigger, Servers = State) ->
    %% Reload server business logic
    io:format("reloader_server: Reloading servers.", []),
    F = fun(#{server_name := ServerName}) ->
                ServerNameStr = ?a2l(ServerName),
                BeamFile = "priv/" ++ ServerNameStr,
                code:purge(ServerName),
                code:delete(ServerName),
                code:load_abs(BeamFile)
        end,
    lists:foreach(F, Servers),
    erlang:send_after(?TIMEOUT, self(), trigger),
    {noreply, State}.

terminate(_Reason, _) ->
    ok.

code_change(_OldVsn, StateData, _Extra) ->
    {ok, StateData}.
