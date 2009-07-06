% Y-combinator in Erlang.
%
% url: http://en.wikipedia.org/wiki/Fixed-point_combinator
% license: public domain
% version: 1.0

-module(y).
-export([y/1]).
-export([test/0]).
-export([test/1]).

y(F) -> G = fun (M) -> F(fun(A) -> (M(M))(A) end) end, G(G).

test(Data) when is_list(Data)->
	io:format("This test function will reverse list ~p and returns the result.~n", [Data]),
	TestFun = fun(F) ->
			fun	({[], Acc}) ->
					Acc;
				({[Head|Tail], Acc}) ->
					F({Tail, [Head] ++ Acc});
				(Array) ->
					F({Array, []})
			end
	end,
	(y:y(TestFun))(Data);

test(_Other) ->
	ok.

test() ->
	ok.
