-module(bank_secondary).

-export([handle_cast/2, handle_call/3, init/1,
         start_link/1, deposit/3, get_balance/2]).

-behavior(gen_server).

-include_lib("eunit/include/eunit.hrl").

-type name() :: atom().

-record(bank_state, { balances :: #{name() => integer()}}).

-record(deposit, { account_name :: name(), amount :: pos_integer() }).

-spec handle_cast(#deposit{}, #bank_state{}) -> {noreply, #bank_state{}}.
handle_cast(#deposit{ account_name = Name, amount = Amount}, BankState) ->
  Balances = BankState#bank_state.balances,
  NewBalances = Balances#{Name => maps:get(Name, BankState#bank_state.balances, 0) + Amount},
  {noreply, BankState#bank_state{balances = NewBalances}}.

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

init(PrimaryPid) ->
    bank:register_secondary(PrimaryPid, self()),
    {ok, #bank_state { balances = #{}}}.

start_link(PrimaryPid) ->
    gen_server:start_link(?MODULE, PrimaryPid, [{debug, [trace]}]).

-spec deposit(pid(), name(), non_neg_integer()) -> ok.
deposit(Pid, Name, Amount) ->
    gen_server:cast(Pid, #deposit{account_name=Name, amount=Amount}),
    ok.

-spec get_balance(pid(), name()) -> {ok, integer()} | {no_account, name()}.
get_balance(Pid, Name) ->
    gen_server:call(Pid, #get_balance{account_name=Name}).

    
