-module(utils).
-export([y/1]).

y(M) ->
	G = fun (F) -> M(fun(A) -> (F(F))(A) end) end,
	G(G).

