-module(bank_supervisor).

-export([init/1, start_link/0]).

-behavior(supervisor).

start_link() ->
    supervisor:start_link(?MODULE, passed_to_init).

init(_) ->
    Flags = #{strategy => rest_for_one,
              % in 3600s nicht mehr als 5 Neustarts
              intensity => 5, period => 3600},
    PrimarySpec = #{id => primary,
                    start => {bank, start_link, []},
                    restart => transient,
                    shutdown => brutal_kill
                    },
    {ok, {Flags, [secondary_spec(s, secondary1), 
                  secondary_spec(s, secondary2),
                  PrimarySpec]}}.

secondary_spec(PrimaryPid, Id) ->
    #{id => Id,
      start => {bank_secondary, start_link, [PrimaryPid]},
      restart => transient,
      shutdown => brutal_kill}.
