-module(np).
-export([]).
-compile(export_all).

myproc() ->
    timer:sleep(5000),
    exit(reason).

chain(0) ->
    receive
        _ -> ok
    after 2000 ->
            exit("chain dies here")
    end;
chain(N) ->
    Pid = spawn(fun() -> chain(N-1) end),
    link(Pid),
    receive
        _ -> ok
    end.


%%%%%%%%%%%%%%%%%%%%
%%   Critic
%%%%%%%%%%%%%%%%%%%%

start_critic() ->
    spawn(?MODULE, critic, []).

start_critic2() ->
    spawn(?MODULE, restarter, []).

restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic, []),
    register(critic, Pid),
    receive
        {'EXIT', Pid, normal} -> ok;
        {'EXIT', Pid, shutdown} -> ok;
        {'EXIT', Pid, _ } -> restarter()
    end.

judge(Band, Album) ->
    Ref = make_ref(),
    critic ! {self(), Ref, {Band, Album}},
    receive
        {Ref, Criticism} -> Criticism
    after 2000 -> timeout
    end.

critic() ->
    receive
        {From, Ref, {"ratm", "ut"}} ->
            From ! {Ref, "Great"};
        {From, Ref, {"soad", "tc"}} ->
            From ! {Ref, "good"};
        {From, Ref, {_Band, _Album}} ->
            From ! {Ref, "Terrible"}
    end,
    critic().
