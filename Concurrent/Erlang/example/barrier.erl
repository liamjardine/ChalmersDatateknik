-module(barrier).
-export([liam/1]).

% initialize barrier for Expected processes
%init(Expected) -> todo.

% Block at Barrier untill all processe have reached it
%wait(Barrier) -> todo.

liam(N) -> N =:= "HEJ".


user() ->
    N = howMany