%% Copyright (c) 2014-2022, Kristian Sällberg
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

-module(tcp_supervisor).
-behavior(supervisor).

-include("brunhilde.hrl").

-export([ start_link/2
        , init/1
        , start_server/1
        , start_socket/4]).

start_link(Servers, Flags) ->
    supervisor:start_link( {local, ?MODULE}
                         , ?MODULE
                         , [Servers, Flags]).

init([Servers, Flags]) ->
    Servers1 = [{Server, Flags} || Server <- Servers],
    lists:foreach(fun start_server/1, Servers1),
    ChildSpec = [#{id       => tcp_supervisor,
                   start    => {tcp_server,
                                start_link,
                                []},
                   restart  => transient,
                   shutdown => 1000,
                   type     => worker,
                   modules  => [tcp_server]}],
    SupFlags = #{strategy  => simple_one_for_one,
                 intensity => 10,
                 period    => 60},
    {ok, {SupFlags, ChildSpec}}.

start_server({#{server_name   := ServerName,
                instance_name := InstanceName,
                port          := Port,
                workers       := Workers,
                transport     := Transport0
               } = Server,
              Flags}) ->
    emit_terminal_box(Port),
    %% Initialize the server
    erlang:apply(ServerName, init, [InstanceName]),
    {Mod, Opts, Transport} =
        case Transport0 of
            http ->
                {gen_tcp, [binary, {reuseaddr, true}, {keepalive, true}], http};
            {https, CertFile, PrivkeyFile, ChainFile} ->
                {ssl, [binary, {reuseaddr, true}, {keepalive, true},
                       {certfile, CertFile}, {keyfile, PrivkeyFile},
                       {cacertfile, ChainFile}], https}
        end,
    {ok, ListenSocket} = Mod:listen(Port, Opts),
    SpawnFun =
        fun() ->
                lists:foreach(
                  fun(_) ->
                          start_socket(ListenSocket, Server, Flags, Transport)
                  end,
                  lists:seq(1, Workers))
        end,
    spawn_link(SpawnFun).

start_socket(ListenSocket, Server, Flags, Transport) ->
    supervisor:start_child(?MODULE, [ListenSocket, Server, Flags, Transport]).

emit_terminal_box(Port) ->
    PortStr = lists:flatten(io_lib:format("~p", [Port])),
    Msg = lists:flatten(io_lib:format("% brunhilde started. "
                                      "Listening at port: ~s. %", [PortStr])),
    Line = [$% || _ <- lists:seq(1, length(Msg))],
    io:format("~n~s~n", [Line]),
    io:format("~s~n",   [Msg]),
    io:format("~s~n",   [Line]).
