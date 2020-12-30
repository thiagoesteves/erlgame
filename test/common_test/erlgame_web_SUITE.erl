%%%-------------------------------------------------------------------
%%% Created : 30 Dec 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc Test suite file
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlgame_web_SUITE).

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

-define(HMTL_OK,             200). %% The request is OK
-define(LOCAL_URL(CMD),      "http://localhost:8080" ++ CMD).
-define(INDEX_HTML_FILE,     "../../../../priv/index.html").

-define(WEBSOCKET_TIMEOUT,   1000).

%%%===================================================================
%%% Test exports
%%%===================================================================
-export([ erlgame_snake_web_index_html_ok/1,
          erlgame_snake_websocket_connect_ok/1]).

all() -> [erlgame_snake_web_index_html_ok,
          erlgame_snake_websocket_connect_ok
         ].

%%%===================================================================
%%% init_per_suite:  Contains common initializations for all test
%%%                  cases in the suite
%%%===================================================================
init_per_suite(Config) ->
  application:ensure_all_started(gproc),
  application:ensure_all_started(gun),
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
%%% Function: erlgame_snake_web_index_html_ok
%%%
%%% Description: Access the cowboys webserver and check it is returning
%%%              the index.html correctly
%%%===================================================================
erlgame_snake_web_index_html_ok(_Config) ->
  %% Start the Application
  application:ensure_all_started(erlgame),
  %% Request Connections
  Request = {?LOCAL_URL("/"), []},
  {ok, {{_Version, ?HMTL_OK, _ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, Request, [], []),

  {ok, Device} = file:read_file(?INDEX_HTML_FILE),

  %% Play Game
  ?assertEqual( erlang:binary_to_list(Device), Body),

  application:stop(erlgame),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_websocket_connect_ok
%%%
%%% Description: Access the cowboy web server and connectes to the
%%%              websocket. The initial message is expected
%%%===================================================================
erlgame_snake_websocket_connect_ok(_Config) ->
  %% Start the Application
  application:ensure_all_started(erlgame),

  %% Connect with the application and open the websockets
  {ok, { ConnPid, StreamRef }} = 
    open_websocket_connection("localhost", 8080, "/websocket"),

  %% Expect the init  message
  ?assertEqual( {ok, {text, <<"Erlgame is alive!">>}}, 
                wait_msg({ ConnPid, StreamRef })),

  gun:shutdown(ConnPid),
  application:stop(erlgame),
  ok.

%%--------------------------------------------------------------------
%% @doc This function opens the websocket in cowboy webserver
%%
%% @param Host Host name
%% @param Port Host port
%% @param WebSocketHost websocket address
%% @end
%%--------------------------------------------------------------------
open_websocket_connection(Host, Port, WebSocketHost) ->
  {ok, ConnPid}   = gun:open(Host, Port, #{protocols => [http], retry => 0}),
  {ok, _Protocol} = gun:await_up(ConnPid),
  StreamRef       = gun:ws_upgrade(ConnPid, WebSocketHost, [], #{compress => true}),
  %% Wait to upgrade connection
  receive
		{gun_upgrade, ConnPid, StreamRef, _, _} ->
			{ok, { ConnPid, StreamRef }};
		Msg1 ->
			{error, {connection_failed, Msg1}}
	after ?WEBSOCKET_TIMEOUT ->
		{error, timeout}
	end.

%%--------------------------------------------------------------------
%% @doc This function waits a message from websocket
%%
%% @param ConnPid Host pid
%% @param StreamRef Websocket reference
%% @param Timeout Timeout to wait for message
%% @end
%%--------------------------------------------------------------------
wait_msg({ ConnPid, StreamRef }, Timeout) ->
	%% Check that we receive the message sent on timer on init.
	receive
		{gun_ws, ConnPid, StreamRef, Msg} ->
			{ok, Msg}
	after Timeout ->
		{error, timeout}
	end.

wait_msg(Arg) ->
	wait_msg(Arg, ?WEBSOCKET_TIMEOUT).

