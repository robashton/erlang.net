-module(dotnet_host_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(HostFxr) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [HostFxr]).

init([HostFxr]) ->
  {ok, { #{ strategy => one_for_one  }, [
                                         #{ id => dotnet_host_bridge
                                            , start => { dotnet_host_control, start_link, [HostFxr] }
                                            , type => worker
                                          }
                                        ]}}.
