-module(messpass).
-compile(export_all).


fact(1) -> 1;
fact(N) -> N * fact(N-1).


start() -> gserver:start(0, fun fact/1).

stop(Server) -> gserver:stop(Server).

math_handler(N, {factorial, M}) -> {reply, N+1, fact(M)};
math_handler(N, status) -> {reply, N, N}.

factorial(Server, M) -> gserver:request(Server, {factorial, M}).

status(Server) -> gserver:request(Server, status).

% node@net