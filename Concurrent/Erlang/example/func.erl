-module(func).
-export([plus/1]).
-export([power/2]).
-export([liamfunc/2]).
-export([map/2]).

plus(0) -> 1;
plus(N) when N > 0, is_integer(N) -> N + 1.

power(_,0) -> 1;
power(X,N) -> X * power(X, N-1).

liamfunc(_,0) -> 11;
liamfunc([H|T],N) -> H+N + liamfunc(T,N-1).  

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].


