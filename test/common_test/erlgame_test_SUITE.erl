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



%%%===================================================================
%%% Test exports
%%%===================================================================
-export([my_first_test_ok/1]).

all() -> [my_first_test_ok
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
%%% Function: my_first_test_ok
%%%
%%% Description: Test the start/stop of the application
%%%===================================================================
my_first_test_ok(_Config) ->

  %% Start the Server
  application:ensure_all_started(erlgame),

  %% Check the server is still running
  ?assertNotEqual( undefined, try_get_state(erlgame_sup) ),

  %% Stop Server
  application:stop(erlgame),

  %% Check the server is not running anymore
  ?assertMatch( undefined, try_get_state(erlgame_sup) ),

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

