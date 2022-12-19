-module(bank_supervisor).

-behavior(supervisor).

init(_) ->
    Flags = #{strategy => rest_for_one,
              % in 3600s nicht mehr als 5 Neustarts
              intensity => 5, period => 3600},
    PrimarySpec = #{id => primary,
                    start => {bank, start_link, []},
                    restart => transient,
                    shutdown => brutal_kill,
                    
                    }
    {ok, {Flags, Children}}.