%%%-------------------------------------------------------------------
%% @doc tracker_business_logic top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tracker_business_logic_sup).
-behaviour(supervisor).

%% Supervisor callbacks
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    ChildSpecs = [
        {package_server, {package_server, start_link, []},
         permanent, 5000, worker, [package_server]}
    ],
    {ok, {{one_for_one, 1, 5}, ChildSpecs}}.

%% internal functions
