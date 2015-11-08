-module(rest_server_app).

-author('kristian@purestyle.se').

-behaviour(application).

-export([start/2, stop/1]).

-define(DEF_PORT, 28251).

-spec start(any(), term()) -> {ok, pid()}
                           | {ok, pid(), term()}
                           | {error, any()}.
start(_Type, _Args) ->
    ets:new(erlrest_global_memory, [public, set, named_table]),
    %% let the user defined module do initialization
    route_handler:init(),
    Listen = get_app_env(listen_port, ?DEF_PORT),
    supervisor:start_link({local, rest_server_supervisor},
                          rest_server_supervisor,
                          [Listen, tcp_reply]).

-spec stop(any()) -> ok.
stop(_State) ->
    ets:delete(erlrest_global_memory),
    ok.

%% Use a standard port number (Default), or consult
%% the ebin/rest_server.app env list, if a listen_port
%% is defined there.
-spec get_app_env(atom(), integer()) -> {ok, [[string()]]} | error.
get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
        {ok, Val} -> Val;
        _ -> case init:get_argument(Opt) of
                 {ok, [[Val|_]]} -> Val;
                 error           -> Default
             end
    end.
