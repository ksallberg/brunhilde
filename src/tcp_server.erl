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

-export([start_link/4]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% State while receiving bytes from the tcp socket
-record(state, { socket        :: port()                 %% client socket
               , server        :: term()                 %% belongs to server
               , flags         :: integer()              %% all flags
               , addr          :: port() | undefined     %% client address
               , data          :: string()               %% collected data
               , body          :: string()               %% only the body
               , body_length   :: integer()              %% total body length
               , route         :: string()               %% route expressed
                                                         %% as string()
               , headers       :: [{string(), string()}] %% HTTP headers
               , parameters    :: [{string(), string()}] %% GET parameters
               , method        :: atom()                 %% method expressed as
                                                         %% atom(), get, post
               , transport     :: atom()
               }).

-type state() :: #state{}.

-define(TIMEOUT, infinity).
-define(OKCODE, "200 OK").

start_link(ListenSocket, Server, Flags, Transport) ->
    gen_server:start_link(?MODULE,
                          [ListenSocket, Server, Flags, Transport],
                          []).

init([Socket, Server, Flags, Transport]) ->
    %% properly seeding the process
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exs1024, {A,B,C}),
    %% Because accepting a connection is a blocking function call,
    %% we can not do it in here. Forward to the server loop!
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket,
                server=Server,
                flags=Flags,
                data="",
                body_length=-1,
                route="unknown",
                parameters=[],
                transport=Transport
               }}.

respond(#state{body = Body, data = _Data0, route = Route,
               method = Method, parameters = Parameters,
               headers = Headers, server = #{server_name := ServName,
                                             instance_name := InstanceName}
              } = State) ->
    Routes = erlang:apply(ServName, routes, []),
    Data   = case Body of
                 [] -> no_data;
                 _  -> Body
             end,
    case [{Proto, HandlerFun} ||
             {Proto, XMethod, XRoute, HandlerFun} <- Routes,
             Route == XRoute andalso Method == XMethod] of
        [] ->
            Answer = case lists:keyfind('*', 1, Routes) of
                         false ->
                             <<"404 error">>;
                         {'*', WildcardFun} ->
                             WildcardFun(Data, Parameters,
                                         Headers, InstanceName)
                     end,
            ok = do_send(State,
                         http_parser:response(Answer, "", "404 NOT FOUND"));
        [{json, HandlerFun}] ->
            Answer = case Data of
                         no_data ->
                             HandlerFun(no_data, Parameters,
                                        Headers, InstanceName);
                         _ ->
                             JsonObj = jsx:decode(?l2b(Data), [return_maps]),
                             HandlerFun(JsonObj, Parameters,
                                        Headers, InstanceName)
                     end,
            {JsonReturn, ExtraHeaders, ReturnCode} =
                case Answer of
                    #{response      := Response,
                      extra_headers := ExtraHeaders0,
                      return_code   := ReturnCode0} ->
                        {jsx:encode(Response), ExtraHeaders0, ReturnCode0};
                    #{response      := Response,
                      extra_headers := ExtraHeaders0} ->
                        {jsx:encode(Response), ExtraHeaders0, ?OKCODE};
                    _ ->
                        {jsx:encode(Answer), "", ?OKCODE}
                end,
            ok = do_send(State, http_parser:response(JsonReturn,
                                                     ExtraHeaders,
                                                     ReturnCode));
        [{xml, HandlerFun}] ->
            Answer = case Data of
                         no_data ->
                             HandlerFun(no_data, Parameters,
                                        Headers, InstanceName);
                         _ ->
                             {XmlObj, _Rest} = xmerl_scan:string(?l2b(Data)),
                             HandlerFun(XmlObj, Parameters,
                                        Headers, InstanceName)
                     end,
            {XmlReturn, ExtraHeaders, ReturnCode} =
                case Answer of
                    #{response      := Response,
                      extra_headers := ExtraHeaders0,
                      return_code   := ReturnCode0} ->
                        {xmerl:export_simple(Response, xmerl_xml),
                         ExtraHeaders0, ReturnCode0};
                    #{response      := Response,
                      extra_headers := ExtraHeaders0} ->
                        {xmerl:export_simple(Response, xmerl_xml),
                         ExtraHeaders0, ?OKCODE};
                    _ ->
                        {xmerl:export_simple(Answer, xmerl_xml), "", ?OKCODE}
                end,
            ok = do_send(State, http_parser:response(XmlReturn,
                                                     ExtraHeaders,
                                                     ReturnCode));
        [{html, HandlerFun}] ->
            Answer = HandlerFun(Data, Parameters, Headers, InstanceName),
            ok     = handle_file_html(Answer, State);
        [{file, HandlerFun}] ->
            Answer = HandlerFun(Data, Parameters, Headers, InstanceName),
            ok     = handle_file_html(Answer, State)
    end,
    do_close(State).

handle_file_html(Answer, State) ->
    {Return, ExtraHeaders, ReturnCode} =
        case Answer of
            #{response      := Response,
              extra_headers := ExtraHeaders0,
              return_code   := ReturnCode0} ->
                {Response, ExtraHeaders0, ReturnCode0};
            #{response      := Response,
              extra_headers := ExtraHeaders0} ->
                {Response, ExtraHeaders0, ?OKCODE};
            _ ->
                {Answer, "", ?OKCODE}
        end,
    do_send(State, http_parser:response(Return,
                                        ExtraHeaders,
                                        ReturnCode)).

-spec handle_cast({data, string()} | timeout | {socket_ready, port()}, state())
    -> {stop, normal, state()} | {noreply, state(), infinity}.
handle_cast(accept, S = #state{socket=ListenSocket,
                               server=Server,
                               flags=Flags,
                               transport=http
                              }) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, AcceptSocket} ->
            tcp_supervisor:start_socket(ListenSocket,
                                        Server,
                                        Flags,
                                        http),
            {noreply, S#state{socket=AcceptSocket}}
    end;
handle_cast(accept, S = #state{socket=ListenSocket,
                               server=Server,
                               flags=Flags,
                               transport=https
                              }) ->
    case ssl:transport_accept(ListenSocket) of
        {ok, NewSocket} ->
            ssl:ssl_accept(NewSocket),
            tcp_supervisor:start_socket(ListenSocket,
                                        Server,
                                        Flags,
                                        https),
            {noreply, S#state{socket=NewSocket}};
        {error, _Reason} ->
            {noreply, big_problem_todo}
    end;

%% Handle the actual client connecting and requesting something
handle_cast({data, Data}, #state{data = DBuf, body_length = _BL} = State) ->
    CurrData = DBuf ++ Data,
    %% When the HTTP request headers have been fully received
    case has_received_headers_end(CurrData) of
        true ->
            {{Method, Route, Params, _HTTPVersion}, Headers, Body}
                = http_parser:parse_request(CurrData),
            NewBL    = get_content_length(Headers),
            NewRoute = Route,
            NewState = State#state{data        = CurrData,
                                   body        = Body,
                                   body_length = NewBL,
                                   route       = NewRoute,
                                   headers     = Headers,
                                   parameters  = Params,
                                   method      = Method},
            case NewBL == length(Body) of
                %% We received entire HTTP request
                true ->
                    respond(NewState),
                    {stop, normal, NewState};
                %% No, wait for more
                false ->
                    {noreply, NewState, ?TIMEOUT}
            end;
        false ->
            NewState = State#state{data = CurrData},
            {noreply, NewState, ?TIMEOUT}
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

handle_info({ssl, _Sock, Bin}, StateData) ->
    ?MODULE:handle_cast({data, Bin}, StateData);

handle_info({tcp_closed, Socket},
            #state{socket=Socket} = StateData) ->
    {stop, normal, StateData};

handle_info({ssl_closed, Socket},
            #state{socket=Socket} = StateData) ->
    {stop, normal, StateData};

handle_info(_Info, StateData) ->
    {noreply, StateData}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, #state{server = #{instance_name := InstanceName},
                          flags  = Flags} = State) ->
    %% Collect statistics
    case tracker_server:ask_for(InstanceName) of
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
    (catch do_close(State)),
    ok.

%% For now, just return the received state data
-spec code_change(atom(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, StateData, _Extra) ->
    {ok, StateData}.

-spec get_content_length([{string(), string()}]) -> integer().
get_content_length(Headers) ->
    case [Len || {"Content-Length", Len} <- Headers] of
        []       -> 0;
        [ConLen] -> {Int, _} = string:to_integer(ConLen),
                    Int
    end.

-compile(export_all).

has_received_headers_end(Data) ->
    do_has_received_headers_end(Data).

do_has_received_headers_end([])        -> false;
do_has_received_headers_end([_])       -> false;
do_has_received_headers_end([_, _])    -> false;
do_has_received_headers_end([_, _, _]) -> false;
do_has_received_headers_end([13, 10, 13, 10 | _]) ->
    true;
do_has_received_headers_end([_ | Xs]) ->
    do_has_received_headers_end(Xs).

do_send(#state{transport=http, socket=Socket}, Message) ->
    try
        gen_tcp:send(Socket, Message)
    catch _:_ ->
        error_logger:error_msg("~p Could not send to socket.~n", [self()])
    end,
    ok;
do_send(#state{transport=https, socket=Socket}, Message) ->
    try
        ssl:send(Socket, Message)
    catch _:_ ->
        error_logger:error_msg("~p Could not send to socket.~n", [self()])
    end,
    ok.

do_close(#state{transport=http, socket=Socket}) ->
    gen_tcp:close(Socket);
do_close(#state{transport=https, socket=Socket}) ->
    ssl:close(Socket).
