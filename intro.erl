-module(intro).
-export([double/1, double/2, is_cute/1,
         dogs_per_legs/1,
         t1/0, t2/0,
         minutes_since_midnight/1,
         list_sum/1,
         m1/0,
         format_server/0, format_loop/0,
         spawn_counter/1, counter/1,
         counter_inc/2, counter_get/1]).
% exportiert eine Funktion test/0
-include_lib("eunit/include/eunit.hrl").

-spec double(number()) -> number().
double(X) -> X * 2.

double(X, Y) -> double(X+Y).

-type pet() :: cat | dog | snake.

% ist ein Haustier niedlich?
% -spec is_cute(cat | dog | snake) -> boolean().
-spec is_cute(pet()) -> boolean().
%is_cute(cat) -> true;
%is_cute(dog) -> true;
%is_cute(snake) -> false.
is_cute(Pet) ->
    case Pet of
        cat -> true;
        dog -> true;
        snake -> false
    end.

% Aggregatzustand von Wasser
-spec state(number()) -> solid | liquid | gas.
state(Temp) ->
    if
        Temp < 0 -> solid;
        Temp > 100 -> gas;
        true -> liquid
    end.

-spec safe_divide(number(), number()) -> {error, divide_by_zero} | {ok, number()}.
safe_divide(X, Y) ->
if
    Y == 0 -> {error, divide_by_zero};
    true -> {ok, X / Y}
end.

dogs_per_legs(Legs) ->
    {ok, Dogs} = safe_divide(Legs, 4),
    Dogs.

-record(time, {hour :: 0..23, minute :: 0..59}).

t1() -> #time{ hour = 11, minute = 34 }.
t2() -> #time{ hour = 13, minute = 12 }.

-spec minutes_since_midnight(#time{}) -> non_neg_integer().
%minutes_since_midnight(#time{ hour = H, minute = M }) ->
%    HM = H * 60,
%    HM + M.
minutes_since_midnight(Time) ->
    Time#time.hour * 60 + Time#time.minute.

-record(dillo, { liveness :: dead | alive, weight :: number() }).
-record(parrot, { sentence :: string(), weight :: number() }).

-type animal() :: #dillo{} | #parrot{}.

d1() -> #dillo { liveness = alive, weight = 10 }.
d2() -> #dillo { liveness = dead, weight = 8 }.
p1() -> #parrot { sentence = "Hello!", weight = 1 }.
p2() -> #parrot { sentence = "Goodbye!", weight = 1.5 }.

-spec run_over_animal(animal()) -> animal().
run_over_animal(#dillo{} = D) ->
    D#dillo { liveness = dead};
run_over_animal(#parrot{ weight = Weight }) ->
    #parrot { sentence = "", weight = Weight}.

% Magie: hört auf _test auf
run_over_animal_test() ->
    ?assertEqual(
      #dillo{ liveness = dead, weight = 11 } ,
       run_over_animal(d1())),
    ?assertEqual(d2(), run_over_animal(d2())).

-spec list_sum(list(number())) -> number().
% list_sum([]) -> 0;
% list_sum([ First | Rest ]) ->
%    First + list_sum(Rest).

list_sum([], Acc) -> Acc;
list_sum([ First | Rest ], Acc) ->
    list_sum(Rest, Acc + First).

list_sum(List) -> list_sum(List, 0).

% Datentypen:
% - Strings: Liste aus Unicode-Scalar-Values
% - Binary: <<1,17,42>> - Byte-Folge
%      ... Pattern-Matching etc. !!!
% - Maps:
m1() -> #{a => "Hello", b => "Goodbye"}.
% https://www.erlang.org/doc/man/maps.html

% Erlang: Programm besteht aus Prozessen, die sich
% gegenseitig Nachrichten schicken.

% Prozess: Code + Queue aus Messages

format_server() ->
    % spawn liefert Prozess-Id
    % spawn(fun format_loop/0).
    % spawn(intro, format_loop, []).
    spawn(?MODULE, format_loop, []).
%    spawn(fun () ->
%        receive
%          Msg ->
%            io:format(Msg)
%        end
%    end).

format_loop() ->
    receive
        shutdown -> ok;
        % https://www.erlang.org/doc/man/io.html#type-format
        Msg -> io:format("message: ~s~n", [Msg]),
               format_loop()
        after 5000 -> % millisekunden
            io:format("waiting ...~n"),
            format_loop()
    end.

counter(Count) ->
    receive
        {inc, Inc} ->
            io:format("counter: ~w, inc: ~w~n", [Count, Inc]),
            counter(Count + Inc);
        {get, Pid} ->
            Pid ! Count,
            counter(Count)
    end.

counter_inc(Pid, Inc) ->
    Pid ! {inc, Inc}.

counter_get(Pid) ->
    Pid ! {get, self()},
    receive 
      Msg -> Msg
    after
        5000 -> timeout
    end.

spawn_counter(Initial) ->
    % Race:
    % Pid = spawn(intro, counter, [Initial]),
    % Prozesse können ihr Schicksal miteinander verbinden
    % link(Pid),
    % Beim Exit eines gelinkten Prozesses sterben wir nicht,
    % sondern bekommen eine Nachricht.
    spawn(fun () ->
            process_flag(trap_exit, true),
            Pid = spawn_link(intro, counter, [Initial]),
            register(counter_service, Pid),
            receive
                {'EXIT', _Pid, _Reason} ->
                    spawn_counter(Initial)
            end
          end).

