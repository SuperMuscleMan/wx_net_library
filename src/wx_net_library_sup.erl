%%%-------------------------------------------------------------------
%% @doc wx_net_library top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(wx_net_library_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
    SupFlags = #{strategy => one_for_all,
                 intensity => 3,
                 period => 60},
    ChildSpecs = [
		#{
			id => wx_http_server,
			start => {wx_http_server, start_link, []},
			restart => permanent,
			shutdown => infinity,
			type => worker,
			modules => [wx_http_server]
		},
		#{
			id => wx_net_server,
			start => {wx_net_server, start_link, []},
			restart => permanent,
			shutdown => infinity,
			type => worker,
			modules => [wx_net_server]
		},
		#{
			id => wx_net_sup,
			start => {wx_net_sup, start_link, []},
			restart => permanent,
			shutdown => infinity,
			type => supervisor,
			modules => [wx_net_sup]
		}
	],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
