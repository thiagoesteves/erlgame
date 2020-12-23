%%%-------------------------------------------------------------------
%%% Created : 22 Dec 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc This file contains the websockets functions
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlgame_wbs_server).

-author('Thiago Esteves').

%%%===================================================================
%%% Includes
%%%===================================================================

-include("erlgame.hrl").
%% For LOG purposes
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Function exports
%%%===================================================================

%% cowboy callbacks
-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

%%%===================================================================
%%% Local Defines
%%%===================================================================

%%%===================================================================
%%% COWBOY local callbacks implementation
%%%===================================================================

websocket_init(State) ->
  ?LOG_INFO("Starting Websocket server at PID: ~p", [self()]),
  {[], State}.

websocket_handle(_Data, State) ->
  {[], State}.

websocket_info(_Info, State) ->
  {[], State}.

%%%===================================================================
%%% API Implementation
%%%===================================================================
-spec init(map(), cowboy:req()) -> {any(), cowboy:req(), any()}.
init(Req0, Opts) ->
  logger:set_module_level(?MODULE, info),
  {cowboy_websocket, Req0, Opts}.

%%====================================================================
%% Internal functions
%%====================================================================