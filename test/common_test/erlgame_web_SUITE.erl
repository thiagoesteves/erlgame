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
-define(SNAKE_GPROC_KEY(User), {p,l,{list_to_atom(User), erlgame_snake_sm, notify_on_update}}).

-define(WEBSOCKET_TIMEOUT,   1000).
-define(DEFAULT_PLAYER,      "Thiago Esteves").

-define(INITIAL_MESSAGE,    <<"Erlgame is alive!">>).
-define(GAME_OVER_MSG,      {close,1000,<<"Game Over">>}).
-define(SNAKE_BEST_PLAYER,  <<"{\"request\":\"get_best_player\",\"game\":\"snake\"}">>).
-define(CREATE_USER(User), list_to_binary("{\"user\":\""++User++"\"}") ).
-define(ACTION_MOVE(User, Move), 
  list_to_binary( 
    "{\"action\":\""++atom_to_list(Move)++"\",\"user\":\""++User++"\"}") ).
-define(BEST_PLAYER_MSG_RESPONSE(User, Points), 
  list_to_binary(
    "{\"best_players\":{\"snake\":{\""++User++"\":"++integer_to_list(Points)++"}}}") ).
-define(BEST_PLAYER_MSG_RESPONSE_EMPTY, 
   <<"{\"best_players\":{\"snake\":{}}}">> ).

%%%===================================================================
%%% Test exports
%%%===================================================================
-export([ erlgame_snake_web_index_html_ok/1,
          erlgame_snake_websocket_connect_ok/1,
          erlgame_snake_websocket_play_ok/1,
          erlgame_snake_websocket_game_over_ok/1,
          erlgame_snake_websocket_best_player_ok/1,
          erlgame_snake_websocket_connect_same_user_ok/1]).

all() -> [erlgame_snake_web_index_html_ok,
          erlgame_snake_websocket_connect_ok,
          erlgame_snake_websocket_play_ok,
          erlgame_snake_websocket_game_over_ok,
          erlgame_snake_websocket_best_player_ok,
          erlgame_snake_websocket_connect_same_user_ok
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
  %% Shutdown
  application:stop(erlgame),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_websocket_connect_ok
%%%
%%% Description: Access the cowboy web server and connects to the
%%%              websocket. The initial message is expected
%%%===================================================================
erlgame_snake_websocket_connect_ok(_Config) ->
  %% Start the Application
  application:ensure_all_started(erlgame),

  %% Connect with the application and open the websockets
  {ok, { ConnPid, StreamRef }} = 
    open_websocket_connection("localhost", 8080, "/websocket"),

  %% Expect the init  message
  ?assertEqual( {ok, {text, ?INITIAL_MESSAGE}}, 
                wait_msg({ ConnPid, StreamRef })),
  %% Shutdown
  gun:shutdown(ConnPid),
  application:stop(erlgame),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_websocket_play_ok
%%%
%%% Description: Access the cowboy web server and connects to the
%%%              websocket. The client creates the player and it
%%%              should be ready to play
%%%===================================================================
erlgame_snake_websocket_play_ok(_Config) ->
  %% Start the Application
  application:ensure_all_started(erlgame),

  %% Connect with the application and open the websockets
  {ok, { ConnPid, StreamRef }} = 
    open_websocket_connection("localhost", 8080, "/websocket"),

  %% Expect the init  message
  {ok, {text, ?INITIAL_MESSAGE}} = wait_msg({ ConnPid, StreamRef }),
  %% Create snake game for the default player
  gun:ws_send(ConnPid, StreamRef, {text, ?CREATE_USER(?DEFAULT_PLAYER)}),
  %% Wait to receive any information from the game
  ?assertMatch( {ok, {text, _}}, wait_msg({ ConnPid, StreamRef })),
  %% Check the game is ready to play
  ?assertMatch( {play, #{user := 'Thiago Esteves'}}, 
                try_get_snake_game_state(?DEFAULT_PLAYER) ),
  %% Shutdown
  gun:shutdown(ConnPid),
  application:stop(erlgame),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_websocket_game_over_ok
%%%
%%% Description: Access the cowboy web server and connects to the
%%%              websocket. After the game is started, it will end up
%%%              in game over state and the client should receive 
%%%              this message
%%%===================================================================
erlgame_snake_websocket_game_over_ok(_Config) ->
  %% Start the Application
  application:ensure_all_started(erlgame),

  %% Connect with the application and open the websockets
  {ok, { ConnPid, StreamRef }} = 
    open_websocket_connection("localhost", 8080, "/websocket"),

  %% Expect the init  message
  {ok, {text, ?INITIAL_MESSAGE}} = wait_msg({ ConnPid, StreamRef }),
  %% Create snake game for the default player
  gun:ws_send(ConnPid, StreamRef, {text, ?CREATE_USER(?DEFAULT_PLAYER)}), 
  %% Wait to receive any information from the game
  ?assertMatch( {ok, {text, _}}, wait_msg({ ConnPid, StreamRef })),
  %% Check the game is ready to play
  ?assertMatch( {play, #{user := 'Thiago Esteves'}}, 
                try_get_snake_game_state(?DEFAULT_PLAYER) ),
  %% Start the moviment
  gun:ws_send(ConnPid, StreamRef, {text, ?ACTION_MOVE(?DEFAULT_PLAYER, ?MOVE_UP)}),
  %% Check game over message was received
  ?assertEqual( ok, wait_until_msg_is_received(?GAME_OVER_MSG) ),
  %% Shutdown
  gun:shutdown(ConnPid),
  application:stop(erlgame),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_websocket_best_player_ok
%%%
%%% Description: Access the cowboy web server and connects to the
%%%              websocket and request the best player. It should come 
%%%              empty and after adding in the database any player, it 
%%%              should return it.
%%%===================================================================
erlgame_snake_websocket_best_player_ok(_Config) ->
  %% Start the Application
  application:ensure_all_started(erlgame),

  %% Connect with the application and open the websockets
  {ok, { ConnPid, StreamRef }} = 
    open_websocket_connection("localhost", 8080, "/websocket"),

  %% Expect the init  message
  {ok, {text, ?INITIAL_MESSAGE}} = wait_msg({ ConnPid, StreamRef }),
  %% Request Best Player
  gun:ws_send(ConnPid, StreamRef, {text, ?SNAKE_BEST_PLAYER}),
  %% No best players
  ?assertEqual( {ok, {text, ?BEST_PLAYER_MSG_RESPONSE_EMPTY }}, 
                 wait_msg({ ConnPid, StreamRef })),
  %% Add Player in Database
  erlgame_db:add_user_points(list_to_atom(?DEFAULT_PLAYER), erlgame_snake_sm, 10),
%% Request Best Player
  gun:ws_send(ConnPid, StreamRef, {text, ?SNAKE_BEST_PLAYER}),
  %% Check new best Player
  ?assertEqual( {ok, {text, ?BEST_PLAYER_MSG_RESPONSE(?DEFAULT_PLAYER, 10)}}, 
                wait_msg({ ConnPid, StreamRef })),
  %% Shutdown
  gun:shutdown(ConnPid),
  application:stop(erlgame),
  ok.

%%%===================================================================
%%% Function: erlgame_snake_websocket_connect_same_user_ok
%%%
%%% Description: Access the cowboy web server and connects two clients
%%%              using the same user. In this case, both must receive
%%%              any message, including game over
%%%===================================================================
erlgame_snake_websocket_connect_same_user_ok(_Config) ->
  %% Start the Application
  application:ensure_all_started(erlgame),

  %% Connect with the application and open the websockets
  {ok, { ConnPid, StreamRef }} = 
    open_websocket_connection("localhost", 8080, "/websocket"),
  %% Expect the init  message
  {ok, {text, ?INITIAL_MESSAGE}} = wait_msg({ ConnPid, StreamRef }),
  %% Create snake game for the default player
  gun:ws_send(ConnPid, StreamRef, {text, ?CREATE_USER(?DEFAULT_PLAYER)}), 
  %% Wait to receive any information from the game
  ?assertMatch( {ok, {text, _}}, wait_msg({ ConnPid, StreamRef })),

  %% Check the game is ready to play
  ?assertMatch( {play, #{user := 'Thiago Esteves'}}, 
                try_get_snake_game_state(?DEFAULT_PLAYER) ),

  %% Connect another client to the same user
  {ok, { ConnPid2, StreamRef2 }} = 
    open_websocket_connection("localhost", 8080, "/websocket"),
  %% Expect the init  message
  {ok, {text, ?INITIAL_MESSAGE}} = wait_msg({ ConnPid2, StreamRef2 }),
  %% Create snake game for the default player again
  gun:ws_send(ConnPid2, StreamRef2, {text, ?CREATE_USER(?DEFAULT_PLAYER)}), 
  %% Wait to receive any information from the game
  ?assertMatch( {ok, {text, _}}, wait_msg({ ConnPid2, StreamRef2 })),

  %% Check there are two Pids registered
  ?assertMatch( [_, _], gproc:lookup_pids(?SNAKE_GPROC_KEY(?DEFAULT_PLAYER)) ),

  %% Start the moviment
  gun:ws_send(ConnPid, StreamRef, {text, ?ACTION_MOVE(?DEFAULT_PLAYER, ?MOVE_UP)}),
  %% Check game over message was received 2 times (Two clients were registered)
  ?assertEqual( ok, wait_until_msg_is_received(?GAME_OVER_MSG) ),
  ?assertEqual( ok, wait_until_msg_is_received(?GAME_OVER_MSG) ),
  %% Shutdown
  gun:shutdown(ConnPid),
  gun:shutdown(ConnPid2),
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

%%--------------------------------------------------------------------
%% @doc This function waits until a specific message is received
%%
%% @param Msg Message to be listened
%% @param Timeout Timeout to wait for message
%% @end
%%--------------------------------------------------------------------
wait_until_msg_is_received(Msg, Timeout)->
  receive
		{gun_ws, _, _, Msg} ->
			ok;
		_ -> wait_until_msg_is_received(Msg, Timeout)
	after Timeout ->
		{error, timeout}
	end.

wait_until_msg_is_received(Arg) ->
	wait_until_msg_is_received(Arg, ?WEBSOCKET_TIMEOUT).

%%--------------------------------------------------------------------
%% @doc This function try to get the state of a registered server
%%
%% @param Name Server name
%% @end
%%--------------------------------------------------------------------
try_get_snake_game_state(Name)->
  try sys:get_state(erlgame_util:maybe_string_to_atom("erlgame_snake_sm:" ++ Name)) of
    S -> S
  catch
    _:_ -> undefined
  end.

