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

%% Possible moviments
-define(MOVE_UP,    up).
-define(MOVE_DOWN,  down).
-define(MOVE_RIGHT, right).
-define(MOVE_LEFT,  left).

-type move() :: up | down | right | left.

-endif. %% erlgame
