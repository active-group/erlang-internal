-module(bank).

-export([handle_cast/2, handle_call/3, handle_info/2, init/1,       
         start_link/0,
         deposit/3, get_balance/2,
         register_secondary/2]).

-behavior(gen_server).

-include_lib("eunit/include/eunit.hrl").

-type name() :: atom().

-record(bank_state, { balances :: #{name() => integer()},
                      secondaries :: list(pid())}).

-record(deposit, { account_name :: name(), amount :: pos_integer() }).
-record(register_secondary, { pid :: pid() }).

-spec handle_cast(#deposit{} | #register_secondary{}, #bank_state{}) -> {noreply, #bank_state{}}.
handle_cast(#deposit{ account_name = Name, amount = Amount}, BankState) ->
  Balances = BankState#bank_state.balances,
  NewBalances = Balances#{Name => maps:get(Name, BankState#bank_state.balances, 0) + Amount},
  lists:foreach(fun (Pid) -> bank_secondary:deposit(Pid, Name, Amount) end, 
                BankState#bank_state.secondaries),
  {noreply, BankState#bank_state{balances = NewBalances}};

handle_cast(#register_secondary{pid = Pid}, BankState) ->
    erlang:monitor(process, Pid),
    {noreply, BankState#bank_state{secondaries = [Pid|BankState#bank_state.secondaries]}}.

handle_info({'DOWN', _, process, Pid, _Reason}, BankState) ->
    {noreply, 
     BankState#bank_state{secondaries = lists:delete(Pid, BankState#bank_state.secondaries)}}.

-record(get_balance, { account_name :: name() }).

-spec handle_call(#get_balance{}, pid(), #bank_state{}) ->
    {reply, {ok, integer()} | {no_account, name()}, #bank_state{}}.
handle_call(#get_balance{ account_name = Name }, _FromPid, BankState) ->
    Result =
        case maps:find(Name, BankState#bank_state.balances) of
          {ok, Balance} -> {ok, Balance};
          error -> {no_account, Name}
        end,
    {reply, Result, BankState}.

init(_) ->
    {ok, #bank_state { balances = #{}, secondaries = []}}.

start_link() ->
    gen_server:start_link(?MODULE, dont_care, [{debug, [trace]}]).

-spec deposit(pid(), name(), non_neg_integer()) -> ok.
deposit(Pid, Name, Amount) ->
    gen_server:cast(Pid, #deposit{account_name=Name, amount=Amount}),
    ok.

-spec get_balance(pid(), name()) -> {ok, integer()} | {no_account, name()}.
get_balance(Pid, Name) ->
    gen_server:call(Pid, #get_balance{account_name=Name}).

-spec register_secondary(pid(), pid()) -> ok.
register_secondary(PrimaryPid, SecondaryPid) ->
    gen_server:cast(PrimaryPid, #register_secondary{pid=SecondaryPid}),
    ok.

simple_test() ->
    {ok, Pid} = start_link(),
    {ok, SecondaryPid1} = bank_secondary:start_link(Pid),
    {ok, SecondaryPid2} = bank_secondary:start_link(Pid),
    deposit(Pid, mike, 100),
    {ok, 100} = get_balance(Pid, mike),
    {ok, 100} = get_balance(SecondaryPid1, mike),
    {ok, 100} = get_balance(SecondaryPid2, mike).

    
