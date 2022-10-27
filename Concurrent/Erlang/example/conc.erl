-module(conc).
-compile(export_all).

print_sum(X,Y) ->
    io:format("~p~n", [X+Y]).

compute_sum(X,Y) -> X + Y.

echo() ->
    receive Msg -> io:format("Recevied: ~p~n", [Msg]) end.


