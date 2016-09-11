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

-module(rest_server_application).
-author('kristian@purestyle.se').

-behaviour(application).
-export([ start/2
        , stop/1
        , do_start/3]).

-include("include/erlrest.hrl").

-spec start(any(), term()) -> {ok, pid()}
                           |  {ok, pid(), term()}
                           |  {error, any()}.
start(_Type, _Args) ->
    {ok, [#{collect_stats  := CollectStats,
            start_observer := StartObserver,
            start_debugger := StartDebugger,
            servers        := Servers}]} = file:consult("brunhilde.conf"),
    case StartDebugger of
        false ->
            do_start(CollectStats, StartObserver, Servers);
        true ->
            debugger:quick(?MODULE, do_start,
                           [CollectStats, StartObserver, Servers])
    end.

do_start(CollectStats, StartObserver, Servers) ->
    Flags = mk_flags([ {CollectStats,  ?COLLECT_STATS}
                     , {StartObserver, ?START_OBSERVER}
                     ]),
    rest_server_supervisor:start_link(Servers, Flags).

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

mk_flags([]) ->
    0;
mk_flags([{true, Flag}|Rest]) ->
    Flag bor mk_flags(Rest);
mk_flags([{false, _}|Rest]) ->
    mk_flags(Rest).
