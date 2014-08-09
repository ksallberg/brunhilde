-module(serv).

-export([start/0]).
-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, terminate/2]).

-behaviour(gen_server).

start() ->
   io:format("~s",["HELLO!"]).


code_change(_,_,_) -> ok.

handle_call(_,_,_) -> ok.

handle_cast(_,_) -> ok.

handle_info(_,_) -> ok.

init(_) -> ok.

terminate(_,_) -> ok.
