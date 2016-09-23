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

-module(tcp_server).

-author('kristian@purestyle.se').

-behaviour(gen_server).

-include("include/brunhilde.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([start_link/3]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% State while receiving bytes from the tcp socket
-record(state, { socket      :: port()                 %% client socket
               , server      :: term()                 %% belongs to server
               , flags       :: integer()              %% all flags
               , addr        :: port() | undefined     %% client address
               , data        :: string()               %% collected data
               , body_length :: integer()              %% total body length
               , route       :: string()               %% route expressed
                                                       %% as string()
               , parameters  :: [{string(), string()}] %% GET parameters
               , method      :: atom()                 %% method expressed as
                                                       %% atom(), get, post
               }).

-type state() :: #state{}.

-define(TIMEOUT, infinity).

-define(SOCK(Msg), {tcp, _Port, Msg}).

start_link(ListenSocket, Server, Flags) ->
    gen_server:start_link(?MODULE, [ListenSocket, Server, Flags], []).

init([Socket, Server, Flags]) ->
    %% properly seeding the process
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exs1024, {A,B,C}),
    %% Because accepting a connection is a blocking function call,
    %% we can not do it in here. Forward to the server loop!
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket, server=Server, flags=Flags,
                data="", body_length=-1, route="unknown", parameters=[]}}.

respond(#state{socket = S, data = Data0, route = Route,
               method = Method, parameters = Parameters,
               server = #{name := ServName}}) ->
    Routes = erlang:apply(ServName, routes, []),
    Data   = case Data0 of
                 [] -> no_data;
                 _  -> Data0
             end,
    case [{Proto, HandlerFun} ||
             {Proto, XMethod, XRoute, HandlerFun} <- Routes,
             Route == XRoute andalso Method == XMethod] of
        [] ->
            Answer = case lists:keyfind('*', 1, Routes) of
                         false ->
                             <<"404 error">>;
                         {'*', WildcardFun} ->
                             WildcardFun(Data, Parameters)
                     end,
            ok     = gen_tcp:send(S, http_parser:response(Answer));
        [{json, HandlerFun}] ->
            Answer = case Data of
                         no_data ->
                             HandlerFun(no_data, Parameters);
                         _ ->
                             JsonObj = jsx:decode(?l2b(Data), [return_maps]),
                             HandlerFun(JsonObj, Parameters)
                     end,
            JsonReturn = jsx:encode(Answer),
            ok         = gen_tcp:send(S, http_parser:response(JsonReturn));
        [{xml, HandlerFun}] ->
            Answer = case Data of
                         no_data ->
                             HandlerFun(no_data, Parameters);
                         _ ->
                             {XmlObj, _Rest} = xmerl_scan:string(?l2b(Data)),
                             HandlerFun(XmlObj, Parameters)
                     end,
            XmlReturn = xmerl:export_simple(Answer, xmerl_xml),
            ok        = gen_tcp:send(S, http_parser:response(XmlReturn));
        [{html, HandlerFun}] ->
            Answer = HandlerFun(Data, Parameters),
            ok     = gen_tcp:send(S, http_parser:response(Answer));
        [{file, HandlerFun}] ->
            Answer = HandlerFun(Data, Parameters),
            ok     = gen_tcp:send(S, http_parser:response(Answer))
    end,
    gen_tcp:close(S).

-spec handle_cast({data, string()} | timeout | {socket_ready, port()}, state())
    -> {stop, normal, state()} | {noreply, state(), infinity}.
handle_cast(accept, S = #state{socket=ListenSocket,
                               server=Server, flags=Flags}) ->
    gen_tcp:controlling_process(ListenSocket, self()),
    ?liof("LISTENSOCK: ~p~n", [ListenSocket]),
    case gen_tcp:accept(ListenSocket) of
        {ok, AcceptSocket} ->
            tcp_supervisor:start_socket(ListenSocket, Server, Flags),
            {noreply, S#state{socket=AcceptSocket}}
    end;

%% Handle the actual client connecting and requesting something
handle_cast({data, Data}, #state{data = DBuf, body_length = BL} = State) ->
    case length(Data ++ DBuf) == BL of
        true ->
            NewState = State#state{data = DBuf ++ Data},
            respond(NewState),
            {stop, normal, NewState};
        false ->
            NewState = case BL of
                -1 ->
                    {{Method, Route, Params, v11}, Headers, Body}
                        = http_parser:parse_request(Data),
                    NewBL     = get_content_length(Headers),
                    NewRoute  = Route,
                    State#state{data        = DBuf ++ Body,
                                body_length = NewBL,
                                route       = NewRoute,
                                parameters  = Params,
                                method      = Method};
                _ ->
                    State#state{data=DBuf ++ Data}
            end,
            case length(NewState#state.data) == NewState#state.body_length of
                true  ->
                    respond(NewState),
                    {stop, normal, NewState};
                false ->
                    {noreply, NewState, ?TIMEOUT}
            end
    end;

handle_cast(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout.~n", [self()]),
    {stop, normal, State}.

-spec handle_call(any(), {pid(), any()}, state()) -> {stop, tuple(), state()}.
handle_call(Request, _From, State) ->
    {stop, {Request, undefined_event}, State}.

-spec handle_info(any(), state() | port()) -> {noreply, state()} |
                                              {stop, normal, state()} |
                                              {noreply, state(), infinity}.
handle_info({tcp, Sock, Bin}, #state{socket=Sock} = StateData) ->
    inet:setopts(Sock, [{active, once}]),
    ?MODULE:handle_cast({data, Bin}, StateData);

handle_info({tcp_closed, Socket},
            #state{socket=Socket, addr=Addr} = StateData) ->
    error_logger:info_msg("~p Client ~p disconnected. ~n", [self(), Addr]),
    {stop, normal, StateData};

handle_info(_Info, StateData) ->
    {noreply, StateData}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, #state{socket = Socket,
                          server = #{name := ServerName},
                          flags  = Flags}) ->
    %% Collect statistics
    case tracker_server:ask_for(ServerName) of
        %% No stats server available
        false ->
            ok;
        %% Send stats, if collect stats has not
        %% explicitly been set to false.
        Pid ->
            case ?flag_set(?COLLECT_STATS, Flags) of
                true ->
                    gen_server:cast(Pid, inc_connections);
                false ->
                    ok
            end
    end,
    (catch gen_tcp:close(Socket)),
    ok.

%% For now, just return the received state data
-spec code_change(atom(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, StateData, _Extra) ->
    {ok, StateData}.

-spec get_content_length([string()]) -> integer().
get_content_length(Headers) ->
    case [Len || [Desc, Len] <- [string:tokens(Header, " ")
              || Header <- Headers], Desc=="Content-Length:"] of
        []     -> 0;
        ConLen -> A = lists:nth(1, ConLen),
                      {Num, _} = string:to_integer(A),
                      Num
    end.
