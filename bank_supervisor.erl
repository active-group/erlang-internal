-module(bank_supervisor).

-behavior(supervisor).

init(_) ->
    Flags = #{}
    {ok, {Flags, Children}}.