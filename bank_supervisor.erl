-module(bank_supervisor).

-behavior(supervisor).

init(_) ->
    Flags = #{strategy => rest_for_one,
}
    {ok, {Flags, Children}}.