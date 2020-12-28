%%%-------------------------------------------------------------------
%%% Created : 14 Dec 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc This file contains the APIs
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlgame_db).

-author('Thiago Esteves').

-behaviour(gen_server).

%%%===================================================================
%%% Includes
%%%===================================================================

-include("erlgame.hrl").

%% For LOG purposes
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Function exports
%%%===================================================================

%% gen_server exports
-export([init/1,
         start_link/0,
         terminate/2,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         code_change/3]).

%% external api
-export([get_user_points/2,
         add_user_points/3,
         get_best_player/1]).

%%%===================================================================
%%% Local Defines
%%%===================================================================

%% Internal database to accumulate the user points
-define(USER_POINTS, user_points).

%% Key value store for the user points
-define(DB_KVS(Id, Game, Points), { {Id, Game}, Points }).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(list()) -> {ok, list()}.
init([]) ->
  logger:set_module_level(?MODULE, error),
  %% Create a cashed table and the database table
  ets:new(?USER_POINTS, [set, named_table]),
  {ok, [] }.

handle_call( { get_user_points, UserId, Game } , _From, State) ->
  Res = get_user_points_priv(UserId, Game),
  {reply, Res, State};

handle_call( { get_best_player, Game } , _From, State) ->
  Res = get_best_player_priv(Game),
  {reply, Res, State}.

handle_cast( { add_user_points, UserId, Game, Points }, State) ->
  add_user_points_priv(UserId, Game, Points),
  {noreply, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

%% @private
terminate(normal, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Public functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc This function returns the current number of points for this 
%%      user. If the user doesn't exist, it will be created.
%%
%% @param UserId User ID name 
%% @param Game Game name 
%% @end
%%--------------------------------------------------------------------
-spec get_user_points(UserId :: atom(), Game :: atom()) -> {ok | error, integer() }.
get_user_points(UserId, Game) when is_atom(UserId), is_atom(Game) ->
  gen_server:call(?MODULE, { get_user_points, UserId, Game } ).

%%--------------------------------------------------------------------
%% @doc This function adds points to the respective user, If the user 
%%      doesn't exist, it will be created.
%%
%% @param UserId User ID name 
%% @param Game Game name 
%% @param Points Number of points to be added
%% @end
%%--------------------------------------------------------------------
-spec add_user_points(UserId :: atom(), Game :: atom(), Points :: integer() ) -> ok.
add_user_points(UserId, Game, Points) 
  when is_atom(UserId), is_atom(Game), is_integer(Points) ->
  gen_server:cast(?MODULE, { add_user_points, UserId, Game, Points } ).

%%--------------------------------------------------------------------
%% @doc This function retrieves the best player for an specific game
%%
%% @param Game Game name 
%% @end
%%--------------------------------------------------------------------
-spec get_best_player(Game :: atom() ) -> { ok, {atom(), integer()} }.
get_best_player(Game) when is_atom(Game) ->
  gen_server:call(?MODULE, { get_best_player, Game } ).

%%====================================================================
%% Internal functions
%%====================================================================

-spec get_user_points_priv(UserId :: atom(), Game :: atom()) -> {ok, integer() }.
get_user_points_priv(UserId, Game) ->
  %% Check the user exist, if not, create one
  case ets:lookup(?USER_POINTS, {UserId, Game}) of
    [?DB_KVS(UserId, Game, Points)] -> {ok, Points};
    [] -> ets:insert(?USER_POINTS, ?DB_KVS(UserId, Game, 0)),
          {ok, 0}
  end.

-spec add_user_points_priv(UserId :: atom(), Game :: atom(), PointsToAdd :: integer() ) -> true.
add_user_points_priv(UserId, Game, PointsToAdd) ->
  %% Check the user exist, if not, create one and add points
  CurrentPoints = case ets:lookup(?USER_POINTS, {UserId, Game}) of
    [?DB_KVS(UserId, Game, Points)] -> Points;
    [] -> 0
  end,
  ets:insert(?USER_POINTS, ?DB_KVS(UserId, Game, CurrentPoints+PointsToAdd)).

-spec get_best_player_priv(Game :: atom()) -> {ok, integer() }.
get_best_player_priv(Game) ->
  %% Search the entire table for the User ID (must be optimized)
  #{players := PlayersList} = ets:foldl(
    fun(?DB_KVS(UserId, GameName, Points), #{ max     := MaxP,
                                              players := Players} = Acc) ->
       case {GameName, Points} of
         {Game, P } when P > MaxP   -> #{max => P, players => [{UserId, P}]};
         {Game, P } when P =:= MaxP -> #{max => P, players => [{UserId, P} | Players]};
         _       -> Acc
       end
    end,
    #{max => 0, players => []},
    ?USER_POINTS),
{ok, PlayersList}.