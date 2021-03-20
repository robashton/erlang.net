-module(erlang_sup).


-export([ start_link/0
        , init/1
        ]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  { ok, { #{ strategy => one_for_all },
        [ #{ id => one, start => { erlang_gen, start_link, [ one ] } },
          #{ id => two, start => { erlang_gen, start_link, [ two ] } },
          #{ id => three, start => { erlang_gen, start_link, [ three ] } }
        ]
        }
     }.
