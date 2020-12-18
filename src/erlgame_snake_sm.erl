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
         start_link/1,
         join_game/1,
         start_game/0]).

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
  GenStatemData = #{ matrix => Matrix,
                     user   => undefined,
                     points => undefined },
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

join({call,From}, { add_user, UserId }, GenStatemData = #{ user := U }) ->
  case U of
    undefined -> ?LOG_DEBUG("Adding new player: ~p", [UserId]);
    UserId    -> ?LOG_DEBUG("Player already in the game: ~p", [UserId]);
            _ -> ?LOG_DEBUG("Exchanging, leaving: ~p and enters: ~p", [U,UserId])
  end,
  %% Capture information from database
  {ok, Points} = erlgame_db:get_user_points(UserId, ?MODULE),
  {keep_state, GenStatemData#{user => UserId, points => Points}, 
   [{reply,From,{ok, {UserId, Points}}}]};

join({call,From}, { start_game }, _GenStatemData = #{ user := undefined }) ->
  ?LOG_WARNING("You need a player to start the game"),
  {keep_state_and_data, [{reply,From,{error, no_player}}]};

join({call,From}, { start_game }, GenStatemData) ->
  ?LOG_INFO("Starting Game"),
  {next_state, play, GenStatemData, [{reply,From,{ok, 0}}]};

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

%%%===================================================================
%%% Public functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc This function adds a user to the game. For this game, only one
%%      user is allowed
%%
%% @param UserId User ID name 
%% @end
%%--------------------------------------------------------------------
-spec join_game(UserId :: list()) -> {ok | error, integer() }.
join_game(UserId) when is_list(UserId) ->
  gen_statem:call(?MODULE, {add_user, maybe_string_to_atom(UserId)}).

%%--------------------------------------------------------------------
%% @doc This function starts the game
%%
%% @param UserId User ID name 
%% @end
%%--------------------------------------------------------------------
-spec start_game() -> {ok | error, integer() }.
start_game() ->
  gen_statem:call(?MODULE, {start_game}).

%%====================================================================
%% Internal functions
%%====================================================================

-spec maybe_string_to_atom(Str :: list()) -> atom().
maybe_string_to_atom(Str) when is_list(Str) ->
  try 
    erlang:list_to_existing_atom(Str)
  catch
    _:_ -> erlang:list_to_atom(Str)
  end.
