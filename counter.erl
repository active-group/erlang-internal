-module(counter).
-export([init/1, handle_cast/2, handle_call/3,
         start_link/1, counter_inc/2, counter_get/1]).

% Dieses Modul muß ein bestimmtes Interface implementierung:
-behavior(gen_server).

% gen_server:start_link(..., ..., Arg)
% -> init(Arg) IM GENSERVER-PROZESS
% <- {ok, InitialState}
% -> loop(..., InitialState)

init(Initial) ->
    {ok, Initial}.

% cast: wir bekommen eine asynchrone Nachricht
% handle_cast(Msg, State)
handle_cast({inc, Inc}, Count) ->
    {noreply, Count + Inc}.

% RPC, es gibt einen Rückgabewert:
handle_call(get, _FromPid, Count) ->
    {reply, 
     Count,  % Antwort
     Count}. % neuer Zustand

% Convenience
start_link(Initial) ->
    gen_server:start_link(?MODULE, Initial, [{debug, [trace]}]).

counter_inc(Pid, Inc) ->
    gen_server:cast(Pid, {inc, Inc}).

counter_get(Pid) ->
    gen_server:call(Pid, get).

% In einem Erlang-Cluster gibt es sternförmig TCP-Verbindungen.
% => _ ! _ zu anderem Knoten ist effektiv blockierend  
% _ ! _ garantiert Reihenfolge, aber nicht Zustellung