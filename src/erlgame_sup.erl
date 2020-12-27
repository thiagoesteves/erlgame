%%%-------------------------------------------------------------------
%%% Created : 14 Dec 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc This is top level supervisor
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlgame_sup).

-author('Thiago Esteves').

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

-export([init/1]).

%%====================================================================
%% Local Definitions
%%====================================================================

-define(SERVER,         ?MODULE).
-define(SNAKE_GAME_SUP, erlgame_snake_sm_sup).

%%====================================================================
%% API functions implementation
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_one,
               intensity => 4,
               period => 30},

  ChildSpecs = [
                  #{id => ?DB_NAME,
                    start => {?DB_NAME, start_link, []},
                    restart => permanent,
                    type => worker,
                    shutdown => brutal_kill},

                  #{id => ?SNAKE_GAME_SUP,
                    start => {?SNAKE_GAME_SUP, start_link, []},
                    restart => permanent,
                    type => supervisor,
                    shutdown => brutal_kill}
                ],
  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
