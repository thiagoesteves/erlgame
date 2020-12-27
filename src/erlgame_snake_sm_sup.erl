%%%-------------------------------------------------------------------
%%% Created : 26 Dec 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc This file contains the supervisor for snake game
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlgame_snake_sm_sup).

-author('Thiago Esteves').

-behaviour(supervisor).

%%%===================================================================
%%% Includes
%%%===================================================================

-include("erlgame.hrl").

%% For LOG purposes
-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API functions
%%====================================================================

-export([start_link/0]).

-export([init/1, create_game/4]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-define(SERVER,          ?MODULE).
-define(SNAKE_GAME_NAME, erlgame_snake_sm).

%%====================================================================
%% API functions implementation
%%====================================================================
-spec start_link() -> {ok, pid()} | {already_started, pid()} | {shutdown, term()} | term().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init(list()) -> {'ok', { #{'intensity'=>non_neg_integer(), 
                                          'period'=>pos_integer(), 
                                          'strategy'=>'one_for_all' | 'one_for_one' | 'rest_for_one' | 'simple_one_for_one'},
                               [ #{'id':=_, 'start':={atom(),atom(),'undefined' | [any()]}, 
                                 'modules'=>'dynamic' | [atom()], 'restart'=>'permanent' | 'temporary' | 'transient', 'shutdown'=>'brutal_kill' |
                                 'infinity' | non_neg_integer(), 'type'=>'supervisor' | 'worker'}]}} | 'ignore'.
init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 5,
                 period => 30},
    %% The child won't start automatically
    ChildSpecs = #{ id => ?SNAKE_GAME_NAME,
                    start => {?SNAKE_GAME_NAME, start_link, []},
                    restart => transient,
                    shutdown => brutal_kill,
                    type => worker },
    {ok, {SupFlags, [ChildSpecs]}}.

%%--------------------------------------------------------------------
%% @doc This function creates a supervised gen_statem to play the
%%      snake game
%%
%% @param UserName User name
%% @param UserPid User pid
%% @param Matrix Game Matrix
%% @param LoopTime updating time
%% @end
%%--------------------------------------------------------------------
-spec create_game(UserName :: string(), UserPid :: pid(),
                  Matrix :: tuple(), LoopTime :: integer()) -> 
  { ok , undefined | pid() }.
create_game(UserName, UserPid, Matrix, LoopTime) ->
  {ok, Pid} = supervisor:start_child(?MODULE, [UserName, UserPid, Matrix, LoopTime]),
  {ok, Pid}.

%%====================================================================
%% Internal functions
%%====================================================================
