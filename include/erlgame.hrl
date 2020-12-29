%%%-------------------------------------------------------------------
%%% Created : 14 Dec 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc Definitions for the whole application
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(erlgame).
-define(erlgame, true).

%%%===================================================================
%%% Global Defines
%%%===================================================================

%% Database Name
-define(DB_NAME, erlgame_db).

%% Snake game definitions
-define(SNAKE_SM_GAME_NAME, erlgame_snake_sm).
-define(SNAKE_SM_UPDATE_MSG(SnakePosition,Points,Food),  
          {snake_sm_updated,SnakePosition,Points,Food}).
-define(SNAKE_SM_GAME_OVER(State),   {snake_sm_game_over, State}).
-define(SNAKE_SM_BEST_PLAYERS(List), {?SNAKE_SM_GAME_NAME, List}).

%% Possible moviments
-define(MOVE_UP,    up).
-define(MOVE_DOWN,  down).
-define(MOVE_RIGHT, right).
-define(MOVE_LEFT,  left).

-type move() :: up | down | right | left.

-endif. %% erlgame
