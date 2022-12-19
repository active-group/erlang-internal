-module(bank_supervisor).

-behavior(supervisor).

init(_) ->
    Flags = #{strategy => }
    {ok, {Flags, Children}}.