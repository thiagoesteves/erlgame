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

-define(TEST_MAX_USERS, 1000).

%%%===================================================================
%%% Test exports
%%%===================================================================
-export([erlgame_start_stop_ok/1,
         erlgame_db_insert_read/1,
         erlgame_db_full_coverage_ok/1]).

all() -> [erlgame_start_stop_ok,
          erlgame_db_insert_read,
          erlgame_db_full_coverage_ok
         ].

%%%===================================================================
%%% init_per_suite:  Contains common initializations for all test
%%%                  cases in the suite
%%%===================================================================
init_per_suite(Config) ->
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
  application:ensure_all_started(erlgame),

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
%%% Function: erlgame_db_full_coverage_ok
%%%
%%% Description: This test will only guarantee 100% coverage for
%%%              erlgame_db.erl
%%%===================================================================
erlgame_db_full_coverage_ok(_Config) ->
  %% Start the Server
  application:ensure_all_started(erlgame),

  erlang:send(?DB_NAME, {none}),
  erlgame_db:code_change(none, none, none),
  erlgame_db:terminate(normal, none),

  ok.

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

