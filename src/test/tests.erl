-module(tests).

-include_lib("eunit/include/eunit.hrl").

-define(test_assembly, "priv/testimpl.dll").
-define(single_process_app, "TestImpl.Tests.SingleProcessApp").


all_test_() ->
  with_bridge(fun(Bridge) ->
                  lists:concat([
                                basic_app:tests(Bridge)
%%                              , type_round_trip:tests(Bridge)
%%                              , record_round_trip:tests(Bridge)
%%                              , basic_gen:tests(Bridge)
%%                              , dotnet_code:tests(Bridge)
                              ])
              end).

create_host() ->
  case ets:whereis(test_host) of
    undefined ->
      { ok, Host } = dotnet:load_hostfxr(),
      ets:new(test_host, [set, protected, named_table]),
      ets:insert(test_host, { host_fxr, Host }),
      Host;
    _ ->
      [{ host_fxr, Host }] =  ets:lookup(test_host, host_fxr),
      Host
  end.

start_bridge(HostFxr) ->
  { ok, Pid } = dotnet_host_bridge:start_link(HostFxr),
  { ok, Bridge } = dotnet_host_bridge:get_bridge(),
  { Pid, Bridge }.

stop_bridge({Pid, _}) ->
  gen_server:stop(Pid).


with_bridge(Fun) ->
  { spawn, [
            { setup
            , fun create_host/0
            , fun (HostFxr) -> [
                   {foreach,
                    fun() -> start_bridge(HostFxr) end,
                    fun stop_bridge/1,
                    [fun({_, Bridge}) -> Fun(Bridge) end]
                   }]
              end
            }
           ]}.

