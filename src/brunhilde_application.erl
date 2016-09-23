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

-module(brunhilde_application).
-author('kristian@purestyle.se').

-behaviour(application).
-export([ start/2
        , stop/1
        , do_start/4]).

-include("include/brunhilde.hrl").

-spec start(any(), term()) -> {ok, pid()}
                           |  {ok, pid(), term()}
                           |  {error, any()}.
start(_Type, _Args) ->
    case file:consult("brunhilde.conf") of
        {error, Reason} ->
            ?liof("Could not load brunhilde.conf: ~p~n", [Reason]),
            CollectStats  = false,
            StartObserver = false,
            StartDebugger = false,
            UseReloader   = false,
            Servers       = [];
        {ok, [#{collect_stats  := CollectStats,
                start_observer := StartObserver,
                start_debugger := StartDebugger,
                use_reloader   := UseReloader,
                servers        := Servers}]} ->
            ok
    end,
    case StartDebugger of
        false ->
            do_start( CollectStats
                    , StartObserver
                    , UseReloader
                    , Servers);
        true ->
            debugger:quick(?MODULE, do_start,
                           [ CollectStats
                           , StartObserver
                           , UseReloader
                           , Servers])
    end.

do_start( CollectStats
        , StartObserver
        , UseReloader
        , Servers) ->
    lager:start(),
    Flags = mk_flags([ {CollectStats,  ?COLLECT_STATS}
                     , {StartObserver, ?START_OBSERVER}
                     , {UseReloader,   ?USE_RELOADER}
                     ]),
    brunhilde_supervisor:start_link(Servers, Flags).

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

mk_flags([]) ->
    0;
mk_flags([{true, Flag}|Rest]) ->
    Flag bor mk_flags(Rest);
mk_flags([{false, _}|Rest]) ->
    mk_flags(Rest).
