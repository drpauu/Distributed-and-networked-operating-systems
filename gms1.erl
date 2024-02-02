-module(gms1).
-export([start/1, start/2, leader/3, slave/4, bcast/3]).

start(Name) ->
    Self = self(),
    spawn_link(fun() -> init(Name, Self) end).

init(Name, Master) ->
    leader(Name, Master, []).

start(Name, Grp) ->
    Self = self(),
    spawn_link(fun() -> init(Name, Grp, Self) end).

init(Name, Grp, Master) ->
    Self = self(),
    Grp ! {join, Self},
    receive
        {view, Leader, Slaves} ->
            Master ! {joined},
            slave(Name, Master, Leader, Slaves)
    end.

leader(Name, Master, Slaves) ->
    receive
        {mcast, Msg} ->
            bcast(Name, {msg, Msg}, Slaves),
            %% Placeholder for future code.
            Master ! {deliver, Msg},
            leader(Name, Master, Slaves);
        {join, Peer} ->
            NewSlaves = lists:append(Slaves, [Peer]),
            bcast(Name, {view, self(), NewSlaves}, NewSlaves),
            leader(Name, Master, NewSlaves);
        stop ->
            ok;
        Error ->
            io:format("leader ~s: strange message ~w~n", [Name, Error])
    end.

bcast(Name, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg end, Nodes).

slave(Name, Master, Leader, Slaves) ->
    receive
        {mcast, Msg} ->
            %% Placeholder for future code.
            Leader ! {mcast, Msg},
            slave(Name, Master, Leader, Slaves);
        {join, Peer} ->
            %% Placeholder for future code.
            Leader ! {join, Peer},
            slave(Name, Master, Leader, Slaves);
        {msg, Msg} ->
            %% Placeholder for future code.
            Master ! {deliver, Msg},
            slave(Name, Master, Leader, Slaves);
        {view, NewLeader, NewSlaves} ->
            %% Update the Leader and Slaves with the new view.
            slave(Name, Master, NewLeader, NewSlaves);
        stop ->
            ok;
        Error ->
            io:format("slave ~s: strange message ~w~n", [Name, Error])
    end.
