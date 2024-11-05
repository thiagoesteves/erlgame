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
         terminate/3,
         callback_mode/0,
         code_change/4]).

%% gen_statem states
-export([join/3,
         play/3,
         game_over/3]).

%% Public API
-export([start_link/3,
         start_game/1,
         action/2]).

%%%===================================================================
%%% Local Defines
%%%===================================================================

%% Default size
-define(DEFAULT_SIZE, {19,19} ).

%% Loop control info
-define(LOOP_MSG,  loop_control).

%% gen_statem definitions
-define(HANDLE_COMMON,
  ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, ?FUNCTION_NAME, D)).

%% Gproc groups
-define(GPROC_PLAYER_GROUP(UserName),  {p,l,{UserName,?MODULE,notify_on_update}}).

-type xy_position() :: {integer(), integer()}.

%% Defines for points
-define(POINTS_BY_LOOP_TIME,  10).
-define(POINTS_BY_EATEN_FOOD, 50).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(UserName :: string(), Matrix :: tuple(), LoopTime :: integer()) -> 
  {ok, pid()} | ignore | {error, term()}.
start_link(UserName, Matrix, LoopTime) when is_list(UserName),
                                            is_tuple(Matrix), 
                                            is_integer(LoopTime)->                                             
  gen_statem:start_link({local,get_registered_name(UserName)}, ?MODULE, 
    [erlgame_util:maybe_string_to_atom(UserName), Matrix, LoopTime], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

-spec init(list()) -> {ok, atom(), map()}.
init([UserId, Matrix, LoopTime]) ->
  logger:set_module_level(?MODULE, error),
  { NextState, GenStatemData } = case erlgame_db:get_game_state(UserId, ?MODULE) of
    { ok, undefined } ->
      { join,
        #{ matrix      => Matrix,
           user        => UserId,
           points      => undefined,
           snake_pos   => [{1,1}],
           last_action => idle,
           loop_time   => LoopTime,
           food        => {0,0} }
      };
    {ok, {OldState, OldGenData} } ->
      % Clear Old State
      erlgame_db:save_game_state(UserId, ?MODULE, undefined),
      {OldState, OldGenData}
  end,
  {ok, NextState, GenStatemData}.

%% @private
terminate(normal, _, _) ->
  ok;

terminate(Reason, State, Data = #{user := UserId}) ->
  ?LOG_ERROR("I'm Terminating now Data: ~p ~p", [Reason, Data]),
  erlgame_db:save_game_state(UserId, ?MODULE, {State, Data}),
  ok.

%% @private
code_change(_OldVsn, State, Data, _Extra) ->
  io:format("( ͡° ͜ʖ ͡°)_/¯ Hot code reloading rocks!!!"),
  {ok, State, Data}.

callback_mode() ->
  [state_functions, state_enter].

%%%============================================================================
%% gen_statem states
%%%============================================================================

%%% JOIN STATE ================================================================
join(enter, _OldState, GenStatemData = #{user := UserId}) ->
  ?LOG_DEBUG("join - enter state"),
  %% Capture information from database
  {ok, Points} = erlgame_db:get_user_points(UserId, ?MODULE),
  {keep_state, GenStatemData#{user => UserId, points => Points}};

join({call,From}, { start_game }, GenStatemData = #{ matrix := {MaxX,MaxY},
                                                     snake_pos := SnakePosition} ) ->
  ?LOG_INFO("Starting Game"),
  %% Create food for the snake
  Food = food_position(MaxX, MaxY, SnakePosition),
  {next_state, play, GenStatemData#{ food => Food }, [{reply,From,{ok, 0}}]};

?HANDLE_COMMON.

%%% JOIN STATE ================================================================
play(enter, _OldState, GenStatemData = #{ loop_time := LoopTime }) ->
  ?LOG_DEBUG("Play - enter state"),
  %% Start the loop control, which will check and play with the user
  erlang:send_after(LoopTime, self(), ?LOOP_MSG),
  {keep_state, GenStatemData};

% Reject reverse movements for snake greater than 1
play(cast, {action, ?MOVE_UP}, GenStatemData = #{last_action := ?MOVE_DOWN,
                                                 snake_pos := [_,_|_] }) ->
  ?LOG_DEBUG("Reverse moviment is not allowed"),
  {keep_state, GenStatemData};
play(cast, {action, ?MOVE_DOWN}, GenStatemData = #{last_action := ?MOVE_UP,
                                                   snake_pos := [_,_|_]}) ->
  ?LOG_DEBUG("Reverse moviment is not allowed"),
  {keep_state, GenStatemData};
play(cast, {action, ?MOVE_RIGHT}, GenStatemData = #{last_action := ?MOVE_LEFT,
                                                    snake_pos := [_,_|_]}) ->
  ?LOG_DEBUG("Reverse moviment is not allowed"),
  {keep_state, GenStatemData};
play(cast, {action, ?MOVE_LEFT}, GenStatemData = #{last_action := ?MOVE_RIGHT,
                                                   snake_pos := [_,_|_]}) ->
  ?LOG_DEBUG("Reverse moviment is not allowed"),
  {keep_state, GenStatemData};
%% Update new action
play(cast, {action, Action}, GenStatemData) ->
  ?LOG_DEBUG("Moving the User"),
  {keep_state, GenStatemData#{ last_action := Action }};
%% In case the game was already started
play({call,From}, { start_game }, GenStatemData) ->
  {keep_state, GenStatemData, [{reply,From,{ok, already_started}}]};

% Execute loop update
play(info, loop_control, GenStatemData = #{ loop_time := LoopTime }) ->
  ?LOG_DEBUG("Play - Action"),
  %% Send message to keep the loop
  erlang:send_after(LoopTime, self(), ?LOOP_MSG),
  case update_user_actions(GenStatemData) of
    {keep_state, NewState} -> %% keep the cycle running
                  {keep_state, NewState};
    {end_game, NewState} -> %% Game Over
                  {next_state, game_over, NewState}
  end;

?HANDLE_COMMON.

%%% JOIN STATE ================================================================
game_over(enter, _OldState, GenStatemData) ->
  ?LOG_DEBUG("Game Over - enter state"),
  notify_game_over(GenStatemData),
  {stop, normal, GenStatemData}.

%%% HANDLE COMMON FUNCTION ====================================================
handle_common(info, _Msg,  State, _GenStatemData) ->
  ?LOG_DEBUG("Info Request - My current State: ~p", [State]),
  keep_state_and_data.

%%%===================================================================
%%% Public functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc This function starts the game
%%
%% @param UserName The respective user name for the game
%% @end
%%--------------------------------------------------------------------
-spec start_game(list()) -> {ok , integer() | already_started }.
start_game(UserName) when is_list(UserName) ->
  gproc:ensure_reg(?GPROC_PLAYER_GROUP(erlgame_util:maybe_string_to_atom(UserName))),
  gen_statem:call(get_registered_name(UserName), {start_game}).

%%--------------------------------------------------------------------
%% @doc This function execute actions for the player
%%
%% @param UserName The respective user name for the game
%% @param Action Action to be executed 
%% @end
%%--------------------------------------------------------------------
-spec action(list(), Action :: move()) -> ok.
action(UserName, Action) when is_list(UserName), is_atom(Action) ->
  gen_statem:cast(get_registered_name(UserName), {action, Action}).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc This function returns an available position to put the food
%%
%% @param MaxX Maximum X value
%% @param MaxY Maximum Y value
%% @param SnakePosition Invalid positions
%% @end
%%--------------------------------------------------------------------
-spec food_position(MaxX :: integer(), MaxY :: integer(), 
                    SnakePosition :: list() ) -> xy_position().
food_position(MaxX, MaxY, SnakePosition) ->
  EmptyPos = generate_board(MaxX,MaxY) -- SnakePosition,
  RandomPosition = rand:uniform(length(EmptyPos)),
  lists:nth(RandomPosition, EmptyPos).

-spec generate_board(MaxX :: integer(), MaxY :: integer()) -> list().
generate_board(MaxX, MaxY) ->
  [{X,Y} || X <- lists:seq(0,MaxX), Y <- lists:seq(0,MaxY)].

%%--------------------------------------------------------------------
%% @doc Update user action and check points
%%
%% @param S Module state
%% @end
%%--------------------------------------------------------------------
-spec update_user_actions(S :: map()) -> {keep_state | end_game, map()}.
update_user_actions(S = #{ last_action := idle, snake_pos := SnakePosition,
                           food := Food, user := User, points := Points}) ->
  ?LOG_DEBUG("User didn't make the first move"),
  notify_players(User, Points, SnakePosition, Food),
  {keep_state,S};
update_user_actions(S = #{ matrix := {MaxX,_}, snake_pos := [{MaxX,_}|_],
                       last_action := ?MOVE_RIGHT}) ->
  {end_game,S};
update_user_actions(S = #{ snake_pos := [{0,_}|_], last_action := ?MOVE_LEFT}) ->
  {end_game,S};
update_user_actions(S = #{ matrix := {_,MaxY}, snake_pos := [{_,MaxY}|_],
                       last_action := ?MOVE_UP}) ->
  {end_game,S};
update_user_actions(S = #{ snake_pos := [{_,0}|_], last_action := ?MOVE_DOWN}) ->
  {end_game,S};
update_user_actions(S = #{ matrix := {MaxX,MaxY}, user := User, points := Points, 
                           snake_pos := SnakePosition, food := Food, 
                           last_action := Action}) ->
  %% Move Snake
  NewSnakePosition = move_snake(SnakePosition, new_head_position(SnakePosition, Action), Food),
  %% Check snake not overlapping
  GameState = check_snake_knot(NewSnakePosition),
  %% Check New if new food is needed
  NewFood = check_food_was_eaten(MaxX,MaxY,NewSnakePosition, Food),
  %% Increase Points (check if Food was eaten)
  AddPoints = case Food of
    NewFood -> ?POINTS_BY_LOOP_TIME;
    _       -> ?POINTS_BY_LOOP_TIME + ?POINTS_BY_EATEN_FOOD
  end,
  NewPoints = Points + AddPoints,
  %% Notify database
  erlgame_db:add_user_points(User, ?MODULE, AddPoints),
  %% Notify web players
  notify_players(User, NewPoints, NewSnakePosition, Food),
  {GameState,S#{snake_pos := NewSnakePosition, food => NewFood, 
                points => NewPoints}}.

%%--------------------------------------------------------------------
%% @doc This function moves the whole sneak based on the new head
%%      head position and check against the food. If the food is 
%%      in the same position of the head, it increments snake size.
%%
%% @param Current snake position
%% @param NewPosition New head position
%% @param NewPosition Food Position
%% @end
%%--------------------------------------------------------------------
-spec move_snake(list(), xy_position(), xy_position())-> list().
move_snake([ Head | [] ], NewPosition, NewPosition) ->
  [NewPosition, Head];
move_snake([ {_,_} | [] ], NewPosition, _) ->
  [NewPosition];
move_snake([{Px,Py} | Tail], NewPosition, NewPosition) ->
  [NewPosition, {Px,Py} | Tail];
move_snake([{Px,Py} | Tail], NewPosition, _) ->
  [NewPosition, {Px,Py} | lists:droplast(Tail)].

%%--------------------------------------------------------------------
%% @doc This function moves the Head position based on the action
%%
%% @param List Snake position
%% @param Move Moviment to be applied to the head
%% @end
%%--------------------------------------------------------------------
-spec new_head_position(list(), move())-> xy_position().
new_head_position([{X,Y}|_],?MOVE_UP) ->
  {X,Y+1};
new_head_position([{X,Y}|_],?MOVE_DOWN) ->
  {X,Y-1};
new_head_position([{X,Y}|_],?MOVE_RIGHT) ->
  {X+1,Y};
new_head_position([{X,Y}|_],?MOVE_LEFT) ->
  {X-1,Y}.

%%--------------------------------------------------------------------
%% @doc This function moves the whole sneak based on the new head
%%      head position and check against the food. If the food is 
%%      in the same position of the head, it increments snake size.
%%
%% @param Current snake position
%% @param NewPosition New head position
%% @param NewPosition Food Position
%% @end
%%--------------------------------------------------------------------
check_food_was_eaten(MaxX,MaxY,[Head|_] = SnakePosition, Head) ->
  food_position(MaxX,MaxY,SnakePosition);
check_food_was_eaten(_,_,_,Food) ->
  Food.

%%--------------------------------------------------------------------
%% @doc This function checks if the snake has overlapped
%%
%% @param List Snake position
%% @end
%%--------------------------------------------------------------------
-spec check_snake_knot(list()) -> end_game | keep_state.
check_snake_knot([Head, _, _, _ | Tail]) ->
  case lists:member(Head, Tail) of
    true ->  end_game;
    false -> keep_state
  end;
check_snake_knot(_) ->
  keep_state.

%%--------------------------------------------------------------------
%% @doc Notify subscribed players the game is over with last State
%%
%% @param GenStatemData Last VAlid State
%%
%% @end
%%--------------------------------------------------------------------
-spec notify_game_over(GenStatemData :: map()) -> ok.
notify_game_over(#{user := User} = GenStatemData) ->
  gproc_notify(?GPROC_PLAYER_GROUP(User), ?SNAKE_SM_GAME_OVER(GenStatemData)).

%%--------------------------------------------------------------------
%% @doc Notify subscribed players the game arena was updated
%%
%% @param UserPid Pid of message destination
%% @param UserPoints Points from the current user
%% @param SnakePosition Snake position list
%% @param Food Food position
%% @end
%%--------------------------------------------------------------------
-spec notify_players(User :: atom(), UserPoints :: integer(), SnakePosition :: list(), Food :: xy_position()) -> ok.
notify_players(User, UserPoints, SnakePosition, Food) ->
  gproc_notify(?GPROC_PLAYER_GROUP(User), ?SNAKE_SM_UPDATE_MSG(SnakePosition, UserPoints, Food)).

%%--------------------------------------------------------------------
%% @doc Retrieves registered snake game name based on the user id
%%
%% @param UserName User name
%% @end
%%--------------------------------------------------------------------
-spec get_registered_name(UserName :: string()) -> atom().
get_registered_name(UserName) ->
  erlgame_util:maybe_string_to_atom(?MODULE_STRING ++ ":" ++ UserName).

%%--------------------------------------------------------------------
%% @doc Gproc function to send messages to the respective pid group
%%
%% @param Group Gproc group to be notified
%% @param Msg Message to be sent

%% @end
%%--------------------------------------------------------------------
-spec gproc_notify(tuple(), tuple()) -> ok.
gproc_notify(Group, Msg) ->
  %% Retrieve Pid List
  Pids = gproc:lookup_pids(Group),
  lists:foreach(
    fun(Pid) ->
      % ?LOG_ERROR("Sending PID: ~p", [Pid]),
      erlang:send(Pid, Msg)
    end,
    Pids).
