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

%% Top level supervisor.

-module(brunhilde_supervisor).
-behavior(supervisor).

-include("include/brunhilde.hrl").

-export([ start_link/2
        , init/1]).

start_link(Servers, Flags) ->
    case ?flag_set(?START_OBSERVER, Flags) of
        true ->
            observer:start();
        false ->
            ok
    end,
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Servers, Flags]).

init([Servers, Flags]) ->
    TrackerSup = #{id       => rest_tracker_supervisor,
                   start    => {tracker_server,
                                start_link,
                                []},
                   restart  => permanent,
                   shutdown => 1000,
                   type     => worker,
                   modules  => [tracker_server]},

    ReloaderSup = #{id       => rest_reloader_supervisor,
                    start    => {reloader_server,
                                 start_link,
                                 [Servers, Flags]},
                    restart  => permanent,
                    shutdown => 1000,
                    type     => worker,
                    modules  => [reloader_server]},

    StatsSup = #{id       => rest_stats_supervisor,
                 start    => {stats_supervisor,
                              start_link,
                              [Servers]},
                 restart  => permanent,
                 shutdown => 1000,
                 type     => supervisor,
                 modules  => [stats_supervisor]},

    TCPSup = #{id       => rest_tcp_supervisor,
               start    => {tcp_supervisor,
                            start_link,
                            [Servers, Flags]},
               restart  => permanent,
               shutdown => 1000,
               type     => supervisor,
               modules  => [tcp_supervisor]},

    SupFlags = #{strategy  => one_for_one,
                 intensity => 10,
                 preiod    => 60},

    {ok, {SupFlags, [ TrackerSup
                    , StatsSup
                    , TCPSup
                    , ReloaderSup]}}.
