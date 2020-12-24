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

%% Define arena game notification message
-define(SNAKE_SM_UPDATE_MSG(SnakePosition,Food),  {snake_sm_updated,SnakePosition,Food}).

%% Possible moviments
-define(MOVE_UP,    up).
-define(MOVE_DOWN,  down).
-define(MOVE_RIGHT, right).
-define(MOVE_LEFT,  left).

-type move() :: up | down | right | left.

-endif. %% erlgame
