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

-module(stats_supervisor).

-behavior(supervisor).

-include("brunhilde.hrl").

-export([ start_link/1
         , start_server/1
         , init/1]).

start_link(Servers) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Servers]).

init([Servers]) ->
    lists:foreach(fun start_server/1, Servers),
    ChildSpec = [#{id       => stats_supervisor,
                   start    => {stats_server,
                                start_link,
                                []},
                   restart  => transient,
                   shutdown => 1000,
                   type     => worker,
                   modules  => [stats_server]}],
    SupFlags = #{strategy  => simple_one_for_one,
                 intensity => 10,
                 preiod    => 60},
    {ok, {SupFlags, ChildSpec}}.

start_server(Server) ->
    SpawnFun = fun () ->
                       supervisor:start_child(?MODULE, [Server])
               end,
    spawn_link(SpawnFun).
