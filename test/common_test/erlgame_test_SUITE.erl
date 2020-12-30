%%%-------------------------------------------------------------------
%%% Created : 14 Dec 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc Test suite file
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlgame_test_SUITE).

%%%===================================================================
%%% Includes
%%%===================================================================

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlgame.hrl").

%%%===================================================================
%%% Function exports
%%%===================================================================

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

%%%===================================================================
%%% local Defines
%%%===================================================================

-define(TEST_MAX_USERS,         1000).
-define(SNAKE_DEFAULT_POSITION, {0,0}).
-define(SNAKE_GAME,             erlgame_snake_sm).
-define(MAX_TIMEOUT_FEED_SNAKE, 10000).
-define(DEFAULT_PLAYER,         "Thiago").
-define(DEFAULT_PLAYER2,        "snake_test").
-define(DEFAULT_PLAYER3,        "user").

-define(SNAKE_GPROC_KEY(User), {p,l,{list_to_atom(User), erlgame_snake_sm, notify_on_update}}).

%%%===================================================================
%%% Test exports
%%%===================================================================
-export([erlgame_start_stop_ok/1,
         erlgame_db_insert_read/1,
         erlgame_db_insert_get_best_player/1,
         erlgame_db_add_non_existent_and_get_best_player/1,
         erlgame_db_full_coverage_ok/1,
         erlgame_snake_join_ok/1,
         erlgame_snake_start_ok/1,
         erlgame_snake_full_coverage_ok/1,
         erlgame_snake_check_idle_ok/1,
         erlgame_snake_already_created_ok/1,
         erlgame_snake_go_right_ok/1,
         erlgame_snake_go_left_ok/1,
         erlgame_snake_go_up_ok/1,
         erlgame_snake_go_down_ok/1,
         erlgame_snake_seek_food_ok/1,
         erlgame_snake_reverse_up_not_allowed_ok/1,
         erlgame_snake_reverse_down_not_allowed_ok/1,
         erlgame_snake_reverse_right_not_allowed_ok/1,
         erlgame_snake_reverse_left_not_allowed_ok/1,
         erlgame_snake_another_observer_ok/1,
         erlgame_snake_crash_join_ok/1,
         erlgame_snake_crash_recovery_state_join_ok/1,
         erlgame_snake_crash_recovery_state_play_ok/1,
         erlgame_snake_knot_game_over_ok/1]).

all() -> [erlgame_start_stop_ok,
          erlgame_db_insert_read,
          erlgame_db_insert_get_best_player,
          erlgame_db_add_non_existent_and_get_best_player,
          erlgame_db_full_coverage_ok,
          erlgame_snake_join_ok,
          erlgame_snake_start_ok,
          erlgame_snake_full_coverage_ok,
          erlgame_snake_check_idle_ok,
          erlgame_snake_already_created_ok,
          erlgame_snake_go_right_ok,
          erlgame_snake_go_left_ok,
          erlgame_snake_go_up_ok,
          erlgame_snake_go_down_ok,
          erlgame_snake_seek_food_ok,
          erlgame_snake_reverse_up_not_allowed_ok,
          erlgame_snake_reverse_down_not_allowed_ok,
          erlgame_snake_reverse_right_not_allowed_ok,
          erlgame_snake_reverse_left_not_allowed_ok,
          erlgame_snake_another_observer_ok,
          erlgame_snake_crash_join_ok,
          erlgame_snake_crash_recovery_state_join_ok,
          erlgame_snake_crash_recovery_state_play_ok,
          erlgame_snake_knot_game_over_ok
         ].

%%%===================================================================
%%% init_per_suite:  Contains common initializations for all test
%%%                  cases in the suite
%%%===================================================================
init_per_suite(Config) ->
  application:ensure_all_started(gproc),
  Config.

%%%===================================================================
%%% end_per_suite: It is called as the final stage of the test suite
%%%                execution
%%%===================================================================
end_per_suite(_Config) ->
 ok.

%%%===================================================================
%%% init_per_testcase: It is called before each test case in the suite.
%%%                    Contains initialization that must be done for
%%%                    each test case.
%%%
%%% @param Name    Name of the test case
%%% @param Config  Config key-value list of runtime configuration data,
%%%                which has the same value as the list returned by
%%%                init_per_suite
%%%===================================================================
init_per_testcase(_, Config) ->
  Config.

%%%===================================================================
%%% end_per_testcase: It is called after each test case has finished,
%%%                   enabling cleanup after init_per_testcase
%%%===================================================================
end_per_testcase(_, _Config) ->
  meck:unload(),
  ok.

%%%===================================================================
%%%           Test case functions: Waiting for OK result
%%%===================================================================

%%%===================================================================
%%% Function: erlgame_start_stop_ok
%%%
%%% Description: Test the start/stop of the application
%%%===================================================================
erlgame_start_stop_ok(_Config) ->

  %% Start the Server
  application:ensure_all_started(erlgame),

  %% Create users, add points, check result
  ?assertNotEqual( undefined, try_get_state(erlgame_sup) ),

  %% Stop Server
  application:stop(erlgame),

  %% Check the server is not running anymore
  ?assertMatch( undefined, try_get_state(erlgame_sup) ),

  ok.

%%%===================================================================
%%% Function: erlgame_db_insert_read
%%%
%%% Description: Test the start/stop of the application
%%%===================================================================
erlgame_db_insert_read(_Config) ->

  %% Start the Server
  erlgame_sup:start_link(),

  UserList = lists:map(
      fun(User) ->
        { erlang:list_to_atom("user:" ++ erlang:integer_to_list(User)),
          ?MODULE,
          rand:uniform(5000) }
      end,
      lists:seq(1,?TEST_MAX_USERS)
    ),

  %% Create users
  lists:foreach(
    fun( {UserId, Game, _Points} )->
      ?assertEqual( {ok, 0}, erlgame_db:get_user_points(UserId, Game) )
    end,
    UserList),

  %% Add random points
  lists:foreach(
    fun( {UserId, Game, Points} )->
      ?assertEqual( ok, erlgame_db:add_user_points(UserId, Game, Points) )
    end,
    UserList),
  
  %% wait the update
  timer:sleep(100),

  %% Check current values
  lists:foreach(
    fun( {UserId, Game, Points} )->
      ?assertEqual( {ok, Points}, erlgame_db:get_user_points(UserId, Game) )
    end,
    UserList),

  ok.

%%%===================================================================
%%% Function: erlgame_db_insert_get_best_player
%%%
%%% Description: Test the read of the best players in an specific game
%%%===================================================================
erlgame_db_insert_get_best_player(_Config) ->

  %% Start the Server
  erlgame_sup:start_link(),

  %% Check best player is empty
  ?assertEqual( {ok, []}, erlgame_db:get_best_player(?MODULE) ),

  ?assertEqual( {ok, 0}, erlgame_db:get_user_points(user_test0, ?MODULE) ),
  ?assertEqual( {ok, 0}, erlgame_db:get_user_points(user_test1, ?MODULE) ),
  ?assertEqual( {ok, 0}, erlgame_db:get_user_points(user_test2, ?MODULE) ),

  %% Add points
  ?assertEqual( ok, erlgame_db:add_user_points(user_test0, ?MODULE, 10) ),
  ?assertEqual( ok, erlgame_db:add_user_points(user_test1, ?MODULE, 20) ),
  ?assertEqual( ok, erlgame_db:add_user_points(user_test2, ?MODULE, 30) ),
  %% wait the update
  timer:sleep(100),

  %% Check best player
  ?assertEqual( {ok, [{user_test2, 30}]}, erlgame_db:get_best_player(?MODULE) ),

  %% Add points to another player to get more best players
  ?assertEqual( ok, erlgame_db:add_user_points(user_test1, ?MODULE, 10) ),
  %% wait the update
  timer:sleep(100),

  %% Check best players
  ExpectedList = lists:usort([{user_test2, 30}, {user_test1,30}]),
  {ok, ReadList} = erlgame_db:get_best_player(?MODULE),
  ?assertEqual( ExpectedList, lists:usort(ReadList) ),
  ok.

%%%===================================================================
%%% Function: erlgame_db_add_non_existent_and_get_best_player
%%%
%%% Description: Test to add non existent players and read the best player
%%%===================================================================
erlgame_db_add_non_existent_and_get_best_player(_Config) ->

  %% Start the Server
  erlgame_sup:start_link(),

  %% Check best player is empty
  ?assertEqual( {ok, []}, erlgame_db:get_best_player(?MODULE) ),

  %% Add points
  ?assertEqual( ok, erlgame_db:add_user_points(user_test0, ?MODULE, 10) ),
  ?assertEqual( ok, erlgame_db:add_user_points(user_test1, ?MODULE, 20) ),
  ?assertEqual( ok, erlgame_db:add_user_points(user_test2, ?MODULE, 30) ),
  ?assertEqual( ok, erlgame_db:add_user_points(user_test2, my_game, 50) ),
  %% wait the update
  timer:sleep(100),
  %% Check best player
  ?assertEqual( {ok, [{user_test2, 50}]}, erlgame_db:get_best_player(my_game) ),
  ok.

%%%===================================================================
%%% Function: erlgame_db_full_coverage_ok
%%%
%%% Description: This test will only guarantee 100% coverage for
%%%              erlgame_db.erl
%%%===================================================================
erlgame_db_full_coverage_ok(_Config) ->
  %% Start the Server
  erlgame_sup:start_link(),

  erlang:send(?DB_NAME, {none}),
  erlgame_db:code_change(none, none, none),
  erlgame_db:terminate(normal, none),

  ok.

%%%===================================================================
%%% Function: erlgame_snake_join_ok
%%%
%%% Description: Create game arena and join a user
%%%===================================================================
erlgame_snake_join_ok(_Config) ->

  %% Start the Server
  erlgame_sup:start_link(),

  %% Create arena game
  {ok, _} = erlgame_snake_sm:start_link(?DEFAULT_PLAYER2, {10,10}, 10),

  %% Create users, add points, check result
  ?assertMatch( {join, #{user := snake_test}}, 
                try_get_state(get_snake_name(?DEFAULT_PLAYER2)) ),

  ok.

%%%===================================================================
%%% Function: erlgame_snake_start_ok
%%%
%%% Description: Create arena game and start
%%%===================================================================
erlgame_snake_start_ok(_Config) ->

  %% Start the Server
  erlgame_sup:start_link(),

  %% Create arena game
  {ok, _} = erlgame_snake_sm:start_link(?DEFAULT_PLAYER2, {10,10}, 10),

  %% Start Game
  ?assertMatch( {ok, _}, erlgame_snake_sm:start_game(?DEFAULT_PLAYER2) ),

  %% Create the state machine
  ?assertMatch( {play, #{user := snake_test}}, 
                try_get_state(get_snake_name(?DEFAULT_PLAYER2)) ),

  ok.

%%%===================================================================
%%% Function: erlgame_snake_check_idle_ok
%%%
%%% Description: Create arena game, start game and check idle
%%%===================================================================
erlgame_snake_check_idle_ok(_Config) ->

  %% Start the Server
  erlgame_sup:start_link(),

  %% Create arena game
  {ok, _} = erlgame_snake_sm:start_link(?DEFAULT_PLAYER3, {10,10}, 10),

  %% Start Game
  ?assertMatch( {ok, _}, erlgame_snake_sm:start_game(?DEFAULT_PLAYER3) ),

  %% Sleep to allow the loop time to occur
  timer:sleep(100),

  %% Create the state machine
  ?assertMatch( {play, #{last_action := idle}},
                 try_get_state(get_snake_name(?DEFAULT_PLAYER3)) ),

  ok.

%%%===================================================================
%%% Function: erlgame_snake_already_created_ok
%%%
%%% Description: Try to create a game for a user that was already 
%%%              created
%%%===================================================================
erlgame_snake_already_created_ok(_Config) ->

  %% Start the Server
  erlgame_sup:start_link(),

  %% Create arena game
  {ok, _} = erlgame_snake_sm:start_link(?DEFAULT_PLAYER3, {10,10}, 10),

  %% Try to create same game with the same user
  ?assertMatch( {error, {already_started, _} }, 
           erlgame_snake_sm:start_link(?DEFAULT_PLAYER3, {10,10}, 10)),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_go_right_ok
%%%
%%% Description: Create arena game, start game and make the first move
%%%===================================================================
erlgame_snake_go_right_ok(_Config) ->
  %% Start the Server
  erlgame_sup:start_link(),
  %% Create arena game
  {ok, _} = erlgame_snake_sm_sup:create_game(?DEFAULT_PLAYER, {10,10}, 1),
  %% Start Game
  erlgame_snake_sm:start_game(?DEFAULT_PLAYER),
  %% Move
  erlgame_snake_sm:action(?DEFAULT_PLAYER, ?MOVE_RIGHT),
  %% Wait for the game over state and check the last state
  ?assertMatch( {game_over, #{last_action := ?MOVE_RIGHT}}, wait_game_over() ),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_go_left_ok
%%%
%%% Description: Create arena game, start game and make the first move
%%%===================================================================
erlgame_snake_go_left_ok(_Config) ->
  %% Start the Server
  erlgame_sup:start_link(),
  %% Create arena game
  {ok, _} = erlgame_snake_sm_sup:create_game(?DEFAULT_PLAYER, {10,10}, 1),
  %% Start Game
  erlgame_snake_sm:start_game(?DEFAULT_PLAYER),
  %% Move
  erlgame_snake_sm:action(?DEFAULT_PLAYER, ?MOVE_LEFT),
  %% Wait for the game over state and check the last state
  ?assertMatch( {game_over, #{last_action := ?MOVE_LEFT}}, wait_game_over() ),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_go_up_ok
%%%
%%% Description: Create arena game, start game and make the first move
%%%===================================================================
erlgame_snake_go_up_ok(_Config) ->
  %% Start the Server
  erlgame_sup:start_link(),
  %% Create arena game
  {ok, _} = erlgame_snake_sm_sup:create_game(?DEFAULT_PLAYER, {10,10}, 1),
  %% Start Game
  erlgame_snake_sm:start_game(?DEFAULT_PLAYER),
  %% Move
  erlgame_snake_sm:action(?DEFAULT_PLAYER, ?MOVE_UP),
  %% Wait for the game over state and check the last state
  ?assertMatch( {game_over, #{last_action := ?MOVE_UP}}, wait_game_over() ),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_go_down_ok
%%%
%%% Description: Create arena game, start game and make the first move
%%%===================================================================
erlgame_snake_go_down_ok(_Config) ->
  %% Start the Server
  erlgame_sup:start_link(),
  %% Create arena game
  {ok, _} = erlgame_snake_sm_sup:create_game(?DEFAULT_PLAYER, {10,10}, 1),
  %% Start Game
  erlgame_snake_sm:start_game(?DEFAULT_PLAYER),
  %% Move
  erlgame_snake_sm:action(?DEFAULT_PLAYER, ?MOVE_DOWN),
  %% Wait for the game over state and check the last state
  ?assertMatch( {game_over, #{last_action := ?MOVE_DOWN}}, wait_game_over() ),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_seek_food_ok
%%%
%%% Description: Create arena game, start game and feed X times
%%%===================================================================
erlgame_snake_seek_food_ok(_Config) ->
  %% Start the Server
  erlgame_sup:start_link(),
  %% Create arena game
  {ok, GamePid} = erlgame_snake_sm:start_link(?DEFAULT_PLAYER3, {5,5}, 1),
  %% Put Snake in the default position
  change_snake_position(GamePid, [?SNAKE_DEFAULT_POSITION]),
  %% Start Game
  erlgame_snake_sm:start_game(?DEFAULT_PLAYER3),
  erlgame_snake_sm:action(?DEFAULT_PLAYER3, ?MOVE_RIGHT),
  %% Wait the snake to be fed
  ?assertEqual(ok, feed_snake(?DEFAULT_PLAYER3, 20) ),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_reverse_up_not_allowed_ok
%%%
%%% Description: Try to reverse a snake bigger than one position
%%%===================================================================
erlgame_snake_reverse_up_not_allowed_ok(_Config) ->
  %% Start the Server
  erlgame_sup:start_link(),
  %% Create arena game
  {ok, GamePid} = erlgame_snake_sm:start_link(?DEFAULT_PLAYER3, {10,10}, 1),
  %% Insert snake bigger then 2 positions to avoid reverse moviment
  change_snake_position(GamePid, [{5,4}, {5,3}]),
  %% Start Game
  erlgame_snake_sm:start_game(?DEFAULT_PLAYER3),
  erlgame_snake_sm:action(?DEFAULT_PLAYER3, ?MOVE_UP),
  erlgame_snake_sm:action(?DEFAULT_PLAYER3, ?MOVE_DOWN),
  %% Wait for the game over state and check the last state
  ?assertMatch( {game_over, #{last_action := ?MOVE_UP}}, wait_game_over() ),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_reverse_down_not_allowed_ok
%%%
%%% Description: Try to reverse a snake bigger than one position
%%%===================================================================
erlgame_snake_reverse_down_not_allowed_ok(_Config) ->
  %% Start the Server
  erlgame_sup:start_link(),
  %% Create arena game
  {ok, GamePid} = erlgame_snake_sm:start_link(?DEFAULT_PLAYER3, {10,10}, 1),
  %% Insert snake bigger then 2 positions to avoid reverse moviment
  change_snake_position(GamePid, [{5,3}, {5,4}]),
  %% Start Game
  erlgame_snake_sm:start_game(?DEFAULT_PLAYER3),
  erlgame_snake_sm:action(?DEFAULT_PLAYER3, ?MOVE_DOWN),
  erlgame_snake_sm:action(?DEFAULT_PLAYER3, ?MOVE_UP),
  %% Wait for the game over state and check the last state
  ?assertMatch( {game_over, #{last_action := ?MOVE_DOWN}}, wait_game_over() ),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_reverse_right_not_allowed_ok
%%%
%%% Description: Try to reverse a snake bigger than one position
%%%===================================================================
erlgame_snake_reverse_right_not_allowed_ok(_Config) ->
  %% Start the Server
  erlgame_sup:start_link(),
  %% Create arena game
  {ok, GamePid} = erlgame_snake_sm:start_link(?DEFAULT_PLAYER3, {10,10}, 1),
  %% Insert snake bigger then 2 positions to avoid reverse moviment
  change_snake_position(GamePid, [{4,3}, {3,3}]),
  %% Start Game
  erlgame_snake_sm:start_game(?DEFAULT_PLAYER3),
  erlgame_snake_sm:action(?DEFAULT_PLAYER3, ?MOVE_RIGHT),
  erlgame_snake_sm:action(?DEFAULT_PLAYER3, ?MOVE_LEFT),
  %% Wait for the game over state and check the last state
  ?assertMatch( {game_over, #{last_action := ?MOVE_RIGHT}}, wait_game_over() ),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_knot_game_over_ok
%%%
%%% Description: Try a knot in the snake and check game over
%%%===================================================================
erlgame_snake_knot_game_over_ok(_Config) ->
  %% Start the Server
  erlgame_sup:start_link(),
  %% Create arena game
  {ok, GamePid} = erlgame_snake_sm:start_link(?DEFAULT_PLAYER3, {20,20}, 1),
  %% Insert snake bigger then 2 positions to avoid reverse moviment
  change_snake_position(GamePid, [{2,5}, {2,4}, {2,3}, {2,2}, {2,1}, {3,1}, {3,2}, {3,3}, {3,4}, {3,5}, {3,6}]),
  %% Start Game
  erlgame_snake_sm:start_game(?DEFAULT_PLAYER3),
  erlgame_snake_sm:action(?DEFAULT_PLAYER3, ?MOVE_RIGHT),
  %% Wait for the game over state and check the last state
  ?assertMatch( {game_over, #{last_action := ?MOVE_RIGHT}}, wait_game_over() ),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_reverse_left_not_allowed_ok
%%%
%%% Description: Try to reverse a snake bigger than one position
%%%===================================================================
erlgame_snake_reverse_left_not_allowed_ok(_Config) ->
  %% Start the Server
  erlgame_sup:start_link(),
  %% Create arena game
  {ok, GamePid} = erlgame_snake_sm:start_link(?DEFAULT_PLAYER3, {10,10}, 1),
  %% Insert snake bigger then 2 positions to avoid reverse moviment
  change_snake_position(GamePid, [{3,3}, {4,3}]),
  %% Start Game
  erlgame_snake_sm:start_game(?DEFAULT_PLAYER3),
  erlgame_snake_sm:action(?DEFAULT_PLAYER3, ?MOVE_LEFT),
  erlgame_snake_sm:action(?DEFAULT_PLAYER3, ?MOVE_RIGHT),
  %% Wait for the game over state and check the last state
  ?assertMatch( {game_over, #{last_action := ?MOVE_LEFT}}, wait_game_over() ),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_another_observer_ok
%%%
%%% Description: create the snake game and add another observer
%%%===================================================================
erlgame_snake_another_observer_ok(_Config) ->
  %% Start the Server
  erlgame_sup:start_link(),
  %% Create arena game
  {ok, GamePid} = erlgame_snake_sm:start_link(?DEFAULT_PLAYER3, {10,10}, 1),
  %% Insert snake bigger then 2 positions to avoid reverse moviment
  change_snake_position(GamePid, [{3,3}, {4,3}]),
  %% Start Game
  erlgame_snake_sm:start_game(?DEFAULT_PLAYER3),
  %% Spawn an observer process
  Pid = spawn_link(fun() -> 
                     erlgame_snake_sm:start_game(?DEFAULT_PLAYER3),
                     receive
                       _ -> timer:sleep(10000)
                     end
                   end),
  %% Sleep to allow the previous process to register
  timer:sleep(100),
  %% Check the both Pids are registered
  ?assertEqual( [self(), Pid], gproc:lookup_pids(?SNAKE_GPROC_KEY(?DEFAULT_PLAYER3)) ),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_crash_join_ok
%%%
%%% Description: create the snake game at join state, crash and 
%%%               expects it to return
%%%===================================================================
erlgame_snake_crash_join_ok(_Config) ->
  %% Start the Server
  erlgame_sup:start_link(),
  %% Create arena game
  {ok, GamePid} = erlgame_snake_sm_sup:create_game(?DEFAULT_PLAYER3, {10,10}, 1),
  %% Insert Snake in a specific position
  change_snake_position(GamePid, [{1,1}]),
  %% Send Kill signal to the snake game
  exit(GamePid, kill),
  %% Check new process was started
  timer:sleep(10),
  %% Check the both Pids are different and the state is still in join
  ?assertNotEqual( GamePid, whereis(get_snake_name(?DEFAULT_PLAYER3)) ),
  ?assertMatch( {join, _}, try_get_state(get_snake_name(?DEFAULT_PLAYER3)) ),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_crash_recovery_state_ok
%%%
%%% Description: create the snake game at join state, crash and 
%%%               expects it to return
%%%===================================================================
erlgame_snake_crash_recovery_state_join_ok(_Config) ->
  %% Start the Server
  erlgame_sup:start_link(),
  %% Create arena game
  {ok, GamePid} = erlgame_snake_sm_sup:create_game(?DEFAULT_PLAYER3, {10,10}, 1),
  %% Insert Snake in a specific position
  change_snake_position(GamePid, [{1,1}]),
  %% Send Kill signal to the snake game
  gen_statem:cast(GamePid, {none}),
  %% Check new process was started
  timer:sleep(10),
  %% Check the both Pids are different and the state is still in join
  ?assertNotEqual( GamePid, whereis(get_snake_name(?DEFAULT_PLAYER3)) ),
  ?assertMatch( {join, _}, try_get_state(get_snake_name(?DEFAULT_PLAYER3)) ),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_crash_recovery_state_play_ok
%%%
%%% Description: create the snake game at join state, crash and 
%%%               expects it to return
%%%===================================================================
erlgame_snake_crash_recovery_state_play_ok(_Config) ->
  %% Start the Server
  erlgame_sup:start_link(),
  %% Create arena game
  {ok, GamePid} = erlgame_snake_sm_sup:create_game(?DEFAULT_PLAYER3, {10,10}, 1),
  %% Insert Snake in a specific position
  change_snake_position(GamePid, [{1,1}]),
  %% Play Game
  erlgame_snake_sm:start_game(?DEFAULT_PLAYER3),
  %% Send Kill signal to the snake game
  gen_statem:cast(GamePid, {none}),
  %% Check new process was started
  timer:sleep(10),
  %% Check the both Pids are different and the state is still in play
  ?assertNotEqual( GamePid, whereis(get_snake_name(?DEFAULT_PLAYER3)) ),
  ?assertMatch( {play, _}, try_get_state(get_snake_name(?DEFAULT_PLAYER3)) ),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_full_coverage_ok
%%%
%%% Description: This test will only guarantee 100% coverage for
%%%              erlgame_db.erl
%%%===================================================================
erlgame_snake_full_coverage_ok(_Config) ->
  %% Start the Server
  erlgame_sup:start_link(),

  {ok, GamePid} = erlgame_snake_sm:start_link(?DEFAULT_PLAYER3, {10,10}, 1),

  %% Join Game (Handle Common)
  erlang:send(GamePid, {none}),
  erlgame_snake_sm:code_change(none, none, none),
  erlgame_snake_sm:terminate(normal, none, none),

  %% Start Game (Handle Common)
  erlgame_snake_sm:start_game(?DEFAULT_PLAYER3),
  erlang:send(GamePid, {none}),

  %% Wait for the game over state
  erlgame_snake_sm:action(?DEFAULT_PLAYER3, ?MOVE_RIGHT),
  ?assertMatch({game_over, _}, wait_game_over()),
  ok.

%%%===================================================================
%%% Function: This function feeds the snake, e. g., makes the snake
%%% run throught the whole arena searching for food.
%%% 
%%%===================================================================
feed_snake(UserName, EatenFood) ->
  feed_snake(UserName, {0,0}, EatenFood, ?MAX_TIMEOUT_FEED_SNAKE).

feed_snake(_,_, _, Timeout) when Timeout =<0 ->
  error;

feed_snake(UserName,LastSnakePosition, EatenFood, Timeout) ->
  %% Get Snake State
  {play, #{ matrix      := Matrix,
            snake_pos   := [Head|Tail],
            last_action := Action} } = try_get_state(get_snake_name(UserName)),
  %% Prepare next Move
  move_snake(UserName,Matrix, LastSnakePosition, Head, Action),
  %% Check if the snake has reached the expected size
  case length(Tail) of
    EatenFood -> ok;
    _         -> timer:sleep(1), % Not yet
                 feed_snake(UserName,Head,EatenFood, Timeout-1)
  end.

%%%===================================================================
%%% Function: This function executes the following snake moviment
%%% 
%%% (3,0)|<<<<<<<<<<
%%% (2,0)|v^>>>>>>>^
%%% (1,0)|V<<<<<<<<<  
%%% (0,0)|>>>>>>>>>^
%%%      (0,1) ...
%%% 
%%% @param User User name
%%% @param {MaxX,MaxY} Matrix
%%% @param {Px,Py} Last head position
%%% @param {Px,Py} Head position
%%% @param Action Last action
%%% @end
%%%===================================================================
move_snake(_,_, {Px,Py}, {Px,Py}, _Action) -> %% No changes yet
  none;
move_snake(UserName,{MaxX,MaxY},_, {Px,Py}, Action) ->
  case {Px,Py,Action} of
    {MaxX,_,?MOVE_RIGHT}    -> erlgame_snake_sm:action(UserName, ?MOVE_UP);
    {MaxX,_,?MOVE_UP}       -> erlgame_snake_sm:action(UserName, ?MOVE_LEFT);
    {1,MaxY,?MOVE_LEFT}     -> none;
    {1,MaxY,?MOVE_UP}       -> none;
    {1,_,?MOVE_LEFT}        -> erlgame_snake_sm:action(UserName, ?MOVE_UP);
    {1,_,?MOVE_UP}          -> erlgame_snake_sm:action(UserName, ?MOVE_RIGHT);
    {0,MaxY,?MOVE_LEFT}     -> erlgame_snake_sm:action(UserName, ?MOVE_DOWN);
    {0,0,?MOVE_DOWN}        -> erlgame_snake_sm:action(UserName, ?MOVE_RIGHT);
    _ -> none
  end.

%%--------------------------------------------------------------------
%% @doc This function move the snake to the required position
%%
%% @param Pid Process Pid to be changed
%% @param Position Position
%% @end
%%--------------------------------------------------------------------
change_snake_position(Pid, Position) ->
  sys:replace_state(Pid, 
    fun({StateM, GenServerState}) -> 
        {StateM, GenServerState#{snake_pos => Position} }
    end).

%%--------------------------------------------------------------------
%% @doc This function waits the game over
%%
%% @param Timeout Maximum timeout to wait
%% @end
%%--------------------------------------------------------------------
wait_game_over(Timeout) ->
  receive
    ?SNAKE_SM_GAME_OVER(State) -> {game_over, State}
  after
    Timeout -> error
  end.

wait_game_over() ->
  wait_game_over(?MAX_TIMEOUT_FEED_SNAKE).

%%--------------------------------------------------------------------
%% @doc This function try to get the state of a registered server
%%
%% @param Name Server name
%% @end
%%--------------------------------------------------------------------
try_get_state(Name)->
  try sys:get_state(Name) of
    S -> S
  catch
    _:_ -> undefined
  end.

get_snake_name(Name) ->
  erlgame_util:maybe_string_to_atom("erlgame_snake_sm:" ++ Name).

