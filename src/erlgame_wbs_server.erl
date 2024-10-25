%%%-------------------------------------------------------------------
%%% Created : 22 Dec 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc This file contains the websockets functions
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlgame_wbs_server).

-author('Thiago Esteves').

%%%===================================================================
%%% Includes
%%%===================================================================

-include("erlgame.hrl").
%% For LOG purposes
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Function exports
%%%===================================================================

%% cowboy callbacks
-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

%%%===================================================================
%%% Local Defines
%%%===================================================================

%%%===================================================================
%%% COWBOY local callbacks implementation
%%%===================================================================

websocket_init(State) ->
  ?LOG_INFO("Starting Websocket server at PID: ~p", [self()]),
  {[{text, <<"Erlgame is alive!">>}], State}.

websocket_handle({text, JsonBin}, State) ->
  ?LOG_INFO("~p", [JsonBin]),
  execute(jsone:decode(JsonBin)),
  {[], State}.

websocket_info(?SNAKE_SM_UPDATE_MSG(S,Points,{Fx,Fy}), State) ->
  %% prepare Json file to be sent throught websockets
  #{ loop_map := NewMap} = lists:foldl(
    fun({X,Y}, #{counter := Acc, loop_map := M}) ->
      #{ snake := Snake} = M,
      #{ counter => Acc+1, 
         loop_map => M#{ snake => Snake#{
           erlgame_util:maybe_string_to_atom("p"++integer_to_list(Acc)) =>
           #{ x => X, y => Y}} }}
    end,
    #{counter => 0, loop_map => #{ food => #{x=> Fx, y => Fy},
                                   points => Points,
                                   snake => #{} }},
    S),
  {[{text,jsone:encode(NewMap#{update => ?SNAKE_SM_GAME_NAME})}], State};

websocket_info(?SNAKE_SM_GAME_OVER(_GenStateData), State) ->
  ?LOG_INFO("Game Over"),
  {reply, {close, 1000, <<"Game Over">>}, State};

websocket_info(?SNAKE_SM_BEST_PLAYERS(PlayersList), State) ->
  NewMap = lists:foldl(
    fun({Name, Points}, #{best_players := #{ ?SNAKE_SM_GAME_NAME := Acc }}) ->
      #{ best_players => #{ ?SNAKE_SM_GAME_NAME => Acc#{ Name => Points }}}
    end,
    #{ best_players => #{ ?SNAKE_SM_GAME_NAME => #{}} },
    PlayersList),
  {[{text,jsone:encode(NewMap)}], State}.

%%%===================================================================
%%% API Implementation
%%%===================================================================
-spec init(map(), cowboy:req()) -> {any(), cowboy:req(), any()}.
init(Req0, Opts) ->
  logger:set_module_level(?MODULE, info),
  {cowboy_websocket, Req0, Opts}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Execute user operations based on the received message from
%%      webclient
%%
%% @param Map Map with client operation
%% @end
%%--------------------------------------------------------------------
-spec execute(map()) -> ok.
execute(#{<<"game">> := <<?SNAKE_SM_GAME_NAME_S>>,
          <<"action">> := Action,<<"user">> := User}) ->
  erlgame_snake_sm:action(b2l(User), binary_to_atom(Action)),
  ok;

execute(#{<<"game">> := <<?SNAKE_SM_GAME_NAME_S>>,
          <<"user">> := User}) ->
  %% Create game and ignore if the game is already created
  case erlgame_snake_sm_sup:create_game(b2l(User), {20,20}, 200) of
    {ok, _} -> none;
    {error, {already_started, _} } -> none
  end,
  {ok, _} = erlgame_snake_sm:start_game(b2l(User)),
  ok;

execute(#{<<"game">> := <<?SNAKE_SM_GAME_NAME_S>>,
          <<"request">> := <<"get_best_player">> }) ->
  {ok, PlayersList} = erlgame_db:get_best_player(?SNAKE_SM_GAME_NAME),
  erlang:send(self(), ?SNAKE_SM_BEST_PLAYERS(PlayersList)),
  ok.

%%--------------------------------------------------------------------
%% @doc convert binary to list
%%
%% @param Bin Binary to be converted
%% @end
%%--------------------------------------------------------------------
b2l(Bin) -> erlang:binary_to_list(Bin).
