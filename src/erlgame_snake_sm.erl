%%%-------------------------------------------------------------------
%%% Created : 18 Dec 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc This file contains the APIs
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlgame_snake_sm).

-author('Thiago Esteves').

-behaviour(gen_statem).

%%%===================================================================
%%% Includes
%%%===================================================================

-include("erlgame.hrl").

%% For LOG purposes
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Function exports
%%%===================================================================

%% gen_statem callbacks
-export([init/1,
         terminate/2,
         callback_mode/0,
         code_change/3]).

%% gen_statem states
-export([join/3,
         play/3,
         finish/3]).

%% Public API
-export([start_link/0,
         start_link/1]).

%%%===================================================================
%%% Local Defines
%%%===================================================================

%% Default size
-define(DEFAULT_SIZE, {50,50} ).

%% Local database name
-define(GAME_INFO, snake_db).

%% gen_statem definitions
-define(HANDLE_COMMON,
  ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, ?FUNCTION_NAME, D)).

-type snake_sm_states() :: join | play | finish.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(Matrix :: tuple()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Matrix) ->
  gen_statem:start_link({local,?MODULE}, ?MODULE, Matrix, []).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_statem:start_link({local,?MODULE}, ?MODULE, ?DEFAULT_SIZE, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

-spec init(tuple()) -> {ok, atom(), map()}.
init(Matrix) ->
  logger:set_module_level(?MODULE, debug),
  %% Create a cashed table and the database table
  ets:new(?GAME_INFO, [set, named_table]),
  GenStatemData = #{ matrix => Matrix },
  {ok, join, GenStatemData}.

%% @private
terminate(normal, _State) ->
  ?LOG_DEBUG("I'm Terminating now"),
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

callback_mode() ->
  [state_functions, state_enter].

%%%============================================================================
%% gen_statem states
%%%============================================================================

%%% JOIN STATE ================================================================
join(enter, _OldState, GenStatemData) ->
  ?LOG_DEBUG("join - enter state"),
  {keep_state, GenStatemData};

join(cast, _MSG, GenStatemData) ->
  ?LOG_DEBUG("join - Action"),
  {keep_state, GenStatemData};

?HANDLE_COMMON.

%%% JOIN STATE ================================================================
play(enter, _OldState, GenStatemData) ->
  ?LOG_DEBUG("Play - enter state"),
  {keep_state, GenStatemData};

play(cast, _MSG, GenStatemData) ->
  ?LOG_DEBUG("Play - Action"),
  {keep_state, GenStatemData};

?HANDLE_COMMON.

%%% JOIN STATE ================================================================
finish(enter, _OldState, GenStatemData) ->
  ?LOG_DEBUG("Finish - enter state"),
  {keep_state, GenStatemData};

finish(cast, _MSG, GenStatemData) ->
  ?LOG_DEBUG("Finish - Action"),
  {keep_state, GenStatemData};

?HANDLE_COMMON.

%%% HANDLE COMMON FUNCTION ====================================================
handle_common({call,From}, Msg, _, _GenStatemData) ->
  ?LOG_DEBUG("CALL message from ~p MSG: ~p", [From, Msg]),
  {keep_state_and_data, [{reply,From,{ok, 0}}]};

handle_common(info, _Msg,  State, _GenStatemData) ->
  ?LOG_DEBUG("Info Request - My current State: ~p", [State]),
  keep_state_and_data;

handle_common(Type, Msg, _, _GenStatemData) ->
  ?LOG_DEBUG("Unexpected message from ~p MSG: ~p", [Type, Msg]),
  keep_state_and_data.

%%====================================================================
%% Internal functions
%%====================================================================
