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
         game_over/3]).

%% Public API
-export([start_link/0,
         start_link/2,
         join_game/1,
         start_game/0,
         action/2]).

%%%===================================================================
%%% Local Defines
%%%===================================================================

%% Default size
-define(DEFAULT_SIZE, {19,19} ).

%% Local database name
-define(GAME_INFO, snake_db).

%% Loop control info
-define(LOOP_MSG,  loop_control).
-define(LOOP_TIME, 1000).

%% gen_statem definitions
-define(HANDLE_COMMON,
  ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, ?FUNCTION_NAME, D)).

-type snake_sm_states() :: join | play | game_over.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(Matrix :: tuple(), LoopTime :: integer()) -> 
  {ok, pid()} | ignore | {error, term()}.
start_link(Matrix, LoopTime) when is_tuple(Matrix), 
                                  is_integer(LoopTime)->
  gen_statem:start_link({local,?MODULE}, ?MODULE, [Matrix, LoopTime], []).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_statem:start_link({local,?MODULE}, ?MODULE, [?DEFAULT_SIZE, ?LOOP_TIME], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

-spec init(list()) -> {ok, atom(), map()}.
init([Matrix, LoopTime]) ->
  logger:set_module_level(?MODULE, debug),
  %% Create a cashed table and the database table
  ets:new(?GAME_INFO, [set, named_table]),
  GenStatemData = #{ matrix      => Matrix,
                     user        => undefined,
                     points      => undefined,
                     position    => {1,1},
                     last_action => idle,
                     loop_time   => LoopTime },
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
play(enter, _OldState, GenStatemData = #{ loop_time := LoopTime }) ->
  ?LOG_DEBUG("Play - enter state"),
  %% Start the loop control, which will check and play with the user
  erlang:send_after(LoopTime, self(), ?LOOP_MSG),
  {keep_state, GenStatemData};

play(cast, {action, UserId, Action}, GenStatemData = #{user := UserId}) ->
  ?LOG_DEBUG("Moving the User"),
  {keep_state, GenStatemData#{ last_action := Action }};

play(info, loop_control, GenStatemData = #{ loop_time := LoopTime }) ->
  ?LOG_DEBUG("Play - Action"),
  case update_user_actions(GenStatemData) of
    {keep_state, NewState} -> %% keep the cycle running
                  erlang:send_after(LoopTime, self(), ?LOOP_MSG),
                  {keep_state, NewState};
    {end_game, NewState} -> %% Game Over
                  {next_state, game_over, NewState}
  end;

?HANDLE_COMMON.

%%% JOIN STATE ================================================================
game_over(enter, _OldState, GenStatemData) ->
  ?LOG_DEBUG("Game Over - enter state"),
  {keep_state, GenStatemData};

game_over(cast, _MSG, GenStatemData) ->
  ?LOG_DEBUG("Game Over - Action"),
  {keep_state, GenStatemData};

?HANDLE_COMMON.

%%% HANDLE COMMON FUNCTION ====================================================
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
  gen_statem:call(?MODULE, 
    {add_user, erlgame_util:maybe_string_to_atom(UserId)}).

%%--------------------------------------------------------------------
%% @doc This function starts the game
%%
%% @param UserId User ID name 
%% @end
%%--------------------------------------------------------------------
-spec start_game() -> {ok | error, integer() }.
start_game() ->
  gen_statem:call(?MODULE, {start_game}).

%%--------------------------------------------------------------------
%% @doc This function execute actions for the player
%%
%% @param UserId User ID name
%% @param Action Action to be executed 
%% @end
%%--------------------------------------------------------------------
-spec action(UserId :: list(), Action :: move()) -> ok.
action(UserId, Action) when is_list(UserId), is_atom(Action) ->
  gen_statem:cast(?MODULE, 
    {action, erlgame_util:maybe_string_to_atom(UserId), Action}).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Update user action and check points
%%
%% @param S Module state
%% @end
%%--------------------------------------------------------------------
-spec update_user_actions(S :: map()) -> {keep_state | end_game, map()}.
update_user_actions(S = #{ last_action := idle }) ->
  ?LOG_DEBUG("User didn't make the first move"),
  {keep_state,S};
update_user_actions(S = #{ matrix := {MaxX,_}, position := {MaxX,_},
                       last_action := ?MOVE_RIGHT}) ->
  {end_game,S};
update_user_actions(S = #{ position := {0,_}, last_action := ?MOVE_LEFT}) ->
  {end_game,S};
update_user_actions(S = #{ matrix := {_,MaxY}, position := {_,MaxY},
                       last_action := ?MOVE_UP}) ->
  {end_game,S};
update_user_actions(S = #{ position := {_,0}, last_action := ?MOVE_DOWN}) ->
  {end_game,S};
update_user_actions(S = #{ user := _User, points := _Points, position := {Px,Py},
                           last_action := ?MOVE_UP}) ->
  {keep_state,S#{position := {Px,Py+1}}};
update_user_actions(S = #{ user := _User, points := _Points, position := {Px,Py},
                           last_action := ?MOVE_DOWN}) ->
  {keep_state,S#{position := {Px,Py-1}}};
update_user_actions(S = #{ user := _User, points := _Points, position := {Px,Py},
                           last_action := ?MOVE_RIGHT}) ->
  {keep_state,S#{position := {Px+1,Py}}};
update_user_actions(S = #{ user := _User, points := _Points, position := {Px,Py},
                           last_action := ?MOVE_LEFT}) ->
  {keep_state,S#{position := {Px-1,Py}}}.
