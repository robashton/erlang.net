-module(dotnet_proclib).

-export([start_link/3
        , init_it/6
        , init_it/7
       , start_link/2]).

%% Look I'm not making apologies for this any more, this whole project is a hacky disaster
%% and I'm honestly quite proud that by digging around the erlang source code I was able to work out
%% how to lie to proc_lib about who our parent is and cancel out the middle-man
%% Once again I am asking for you to not use this project for anything, ever, any time.

start_link(Module, Args) ->
  { ok, ActualOwnerPid } = dotnet_host_bridge:get_owner_of_dispatch(self()),
  InitArgs = [ gen_server
               , self()
               , ActualOwnerPid
               , Module
               , Args
               , []
             ],
  start_link(InitArgs).

start_link(Name, Module, Args) ->
  { ok, ActualOwnerPid } = dotnet_host_bridge:get_owner_of_dispatch(self()),
  InitArgs = [ gen_server
               , self()
               , ActualOwnerPid
               , Name
               , Module
               , Args
               , []
             ],
  start_link(InitArgs).


start_link(Args) ->
  { ok, ActualOwnerPid } = dotnet_host_bridge:get_owner_of_dispatch(self()),
  Parent = get_my_name(ActualOwnerPid),
  Ancestors = get_ancestors(ActualOwnerPid),

  Pid = erlang:spawn(proc_lib, init_p, [Parent,Ancestors, dotnet_proclib, init_it, Args]),

  receive
    {ack, Pid, Return} ->
      Return;

    {'EXIT', Pid, Reason} ->
      {error, Reason}
  end.

get_my_name(ActualOwnerPid) ->
  case erlang:process_info(ActualOwnerPid,registered_name) of
    {registered_name,Name} -> Name;
    _  -> ActualOwnerPid
  end.

get_ancestors(ActualOwnerPid) ->
  {dictionary, Dict} = erlang:process_info(ActualOwnerPid, dictionary),
  case lists:keyfind('$ancestors', 1, Dict) of
    { '$ancestors', A } when is_list(A) -> A;
    false -> []
  end.

where({global, Name}) -> global:whereis_name(Name);
where({via, Module, Name}) -> Module:whereis_name(Name);
where({local, Name})  -> whereis(Name).

init_it(GenMod, Starter, Parent, Mod, Args, Options) ->
  link(Parent),
  gen:init_it(GenMod, Starter, Parent, Mod, Args, Options).

init_it(GenMod, Starter, Parent, Name, Mod, Args, Options) ->
  link(Parent),
  gen:init_it(GenMod, Starter, Parent, Name, Mod, Args, Options).
