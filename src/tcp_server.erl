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

-module(tcp_server).

-author('kristian@purestyle.se').

-behaviour(gen_server).

-include("brunhilde.hrl").

-export([start_link/4, has_received_headers_end/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% State while receiving bytes from the tcp socket
-record(state, { %% client socket
                 socket        :: port() | undefined,
                 %% belongs to server
                 server        :: term(),
                 %% all flags
                 flags         :: integer(),
                 %% client address
                 addr          :: port() | undefined,
                 %% collected data
                 data          :: binary(),
                 %% only the body
                 body          :: binary() | undefined,
                 %% total body length
                 body_length   :: integer(),
                 %% route expressed as string()
                 route         :: string(),
                 %% HTTP headers
                 headers       :: [{binary(), binary()}] | undefined,
                 %% GET parameters method
                 %% expressed as atom(), get, post
                 parameters    :: [{binary(), binary()}],
                 method        :: atom(),
                 transport     :: atom()
               }).

-type state() :: #state{}.

-define(TIMEOUT, infinity).
-define(OKCODE, <<"200 OK">>).

start_link(ListenSocket, Server, Flags, Transport) ->
    gen_server:start_link(?MODULE,
                          [ListenSocket, Server, Flags, Transport],
                          []).

-spec init([term()]) -> {ok, #state{}}.
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
                data= <<"">>,
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
                 <<"">> -> no_data;
                 _  -> Body
             end,
    Subdomain = http_parser:get_subdomain(Headers),
    case [{Proto, HandlerFun} ||
             #route{protocol = Proto,
                    verb = XMethod,
                    address = XRoute,
                    subdomain = XSubdomain,
                    callback = HandlerFun} <- Routes,
             Route == XRoute andalso
                 Method == XMethod andalso
                 Subdomain == XSubdomain] of
        [] ->
            Answer = case lists:keyfind('*', 1, Routes) of
                         false ->
                             <<"404 error">>;
                         {'*', WildcardFun} ->
                             WildcardFun(Data, Parameters,
                                         Headers, InstanceName)
                     end,
            ok = do_send(State,
                         http_parser:response(Answer,
                                              <<"">>,
                                              <<"404 NOT FOUND">>));
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
                {Answer, <<"">>, ?OKCODE}
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
    SpawnFun =
        fun() ->
                tcp_supervisor:start_socket(ListenSocket, Server, Flags, http)
        end,
    case gen_tcp:accept(ListenSocket) of
        {ok, AcceptSocket} ->
            SpawnFun(),
            {noreply, S#state{socket=AcceptSocket}};
        {error, Reason} ->
            SpawnFun(),
            CloseState = S#state{socket=undefined},
            {stop, Reason, CloseState}
    end;

handle_cast(accept, S = #state{socket=ListenSocket,
                               server=Server,
                               flags=Flags,
                               transport=https
                              }) ->
    %% When we accept this server, we create a new one to
    %% serve the next upcoming request.
    %% If ssl:transport_accept/1 or ssl:ssl_accept/1 fails
    %% then we kill the current process, so that it does not
    %% stay hanging forever.
    SpawnFun = fun() ->
                       tcp_supervisor:start_socket(ListenSocket,
                                                   Server,
                                                   Flags,
                                                   https)
               end,
    case ssl:transport_accept(ListenSocket) of
        {ok, NewSocket} ->
            case ssl:handshake(NewSocket) of
                {ok, SslSocket} ->
                    SpawnFun(),
                    {noreply, S#state{socket=SslSocket}};
                {error, Reason} ->
                    SpawnFun(),
                    error_logger:error_msg("tcp_server: Error ~p~n", [Reason]),
                    %% We want to close the new socket, not the
                    %% listening socket
                    CloseState = S#state{socket=NewSocket},
                    {stop, normal, CloseState}
            end;
        {error, Reason} ->
            SpawnFun(),
            error_logger:error_msg("tcp_server: Error ~p~n", [Reason]),
            CloseState = S#state{socket=undefined},
            {stop, normal, CloseState}
    end;

%% Handle the actual client connecting and requesting something
%%
%% Waiting for more body data:
handle_cast({data, Data}, #state{body_length = BL, body = Body} = State)
  when BL > 0 ->
    NewBody = <<Body/binary, Data/binary>>,
    NewState = State#state{body=NewBody},
    case byte_size(NewBody) == BL of
        %% We received entire HTTP request
        true ->
            respond(NewState),
            {stop, normal, NewState};
        %% No, wait for more
        false ->
            {noreply, NewState, ?TIMEOUT}
    end;
%% Waiting for headers and body
handle_cast({data, Data}, #state{data = DBuf, body_length = -1} = State) ->
    NewData = <<DBuf/binary, Data/binary>>,
    %% When the HTTP request headers have been fully received
    case has_received_headers_end(NewData) of
        true ->
            case http_parser:parse_request(NewData) of
                {Err, Why} ->
                    error_logger:error_msg("tcp_server: Error ~p ~p ~p ~n",
                                           [Err, Why, NewData]),
                    Answer = <<"404 error">>,
                    ok = do_send(State,
                                 http_parser:response(Answer,
                                                      <<"">>,
                                                      <<"404 NOT FOUND">>)),
                    {stop, normal, State};
                {{Method, Route, Params, _HTTPVersion}, Headers, Body} ->
                    NewBL = get_content_length(Headers),
                    NewRoute = Route,
                    NewState = State#state{data        = NewData,
                                           body        = Body,
                                           body_length = NewBL,
                                           route       = NewRoute,
                                           headers     = Headers,
                                           parameters  = Params,
                                           method      = Method},
                    case byte_size(Body) == NewBL of
                        %% We received entire HTTP request
                        true ->
                            respond(NewState),
                            {stop, normal, NewState};
                        %% No, wait for more
                        false ->
                            {noreply, NewState, ?TIMEOUT}
                    end
            end;
        false ->
            NewState = State#state{data = NewData},
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
terminate(_Reason, #state{} = State) ->
    catch do_close(State),
    ok.

%% For now, just return the received state data
-spec code_change(atom(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, StateData, _Extra) ->
    {ok, StateData}.

-spec get_content_length([{binary(), binary()}]) -> integer().
get_content_length(Headers) ->
    case lists:keysearch(<<"Content-Length">>, 1, Headers) of
        false ->
            0;
        {value, {_, ConLen}} ->
            binary_to_integer(ConLen)
    end.

has_received_headers_end(Data) ->
    do_has_received_headers_end(Data).

do_has_received_headers_end(X) when (byte_size(X) < 4) ->
    false;
do_has_received_headers_end(<<"\r\n\r\n", _More/binary>>) ->
    true;
do_has_received_headers_end(<<_X,Xs/binary>>) ->
    do_has_received_headers_end(Xs).

do_send(#state{transport=Transport, socket=Socket}, Message) ->
    Mod = case Transport of
              http -> gen_tcp;
              https -> ssl
          end,
    try
        Mod:send(Socket, Message)
    catch _:_ ->
            error_logger:error_msg("~p Could not send to socket.~n", [self()])
    end,
    ok.

do_close(#state{socket=undefined}) ->
    %% an error has occurred, do not close
    ok;
do_close(#state{transport=http, socket=Socket}) ->
    gen_tcp:close(Socket);
do_close(#state{transport=https, socket=Socket}) ->
    ssl:close(Socket).
