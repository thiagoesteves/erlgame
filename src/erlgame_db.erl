%%%-------------------------------------------------------------------
%%% Created : 14 Dec 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc This file contains the APIs
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlgame_db).

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

%% gen_server exports
-export([init/1,
         start_link/0,
         terminate/2,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         code_change/3]).

%% external api
-export([get_user_points/1,
         add_user_points/2]).

%%%===================================================================
%%% Local Defines
%%%===================================================================

%% Internal database to accumulate the user points
-define(USER_POINTS, user_points).

%% Key value store for the user points
-define(DB_KVS(Id, Points), { Id, {Points} }).

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

handle_call( { get_user_points, UserId } , _From, State) ->
  Res = get_user_points_priv(UserId),
  {reply, Res, State}.

handle_cast( { add_user_points, UserId, Points }, State) ->
  add_user_points_priv(UserId, Points),
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
%% @end
%%--------------------------------------------------------------------
-spec get_user_points(UserId :: atom()) -> {ok | error, integer() }.
get_user_points(UserId) when is_atom(UserId) ->
  gen_server:call(?MODULE, { get_user_points, UserId } ).

%%--------------------------------------------------------------------
%% @doc This function adds points to the respective user
%%
%% @param UserId User ID name 
%% @param Points Number of points to be added
%% @end
%%--------------------------------------------------------------------
-spec add_user_points(UserId :: atom(), Points :: integer() ) -> ok.
add_user_points(UserId, Points) when is_atom(UserId), is_integer(Points) ->
  gen_server:cast(?MODULE, { add_user_points, UserId, Points } ).

%%====================================================================
%% Internal functions
%%====================================================================

-spec get_user_points_priv(UserId :: atom()) -> {ok, integer() }.
get_user_points_priv(UserId) ->
  %% Check the user exist, if not, create one
  case ets:lookup(?USER_POINTS, UserId) of
    [?DB_KVS(UserId, Points)] -> {ok, Points};
    [] -> ets:insert(?USER_POINTS, ?DB_KVS(UserId, 0)),
          {ok, 0}
  end.

-spec add_user_points_priv(UserId :: atom(), Points :: integer() ) -> true.
add_user_points_priv(UserId, Points) ->
  %% Retrieve current number of points
  [?DB_KVS(UserId, CurrentPoints)] = ets:lookup(?USER_POINTS, UserId),
  ets:insert(?USER_POINTS, ?DB_KVS(UserId, CurrentPoints+Points)).
