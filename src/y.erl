%% Y-combinator in Erlang.
%%
%% url: http://en.wikipedia.org/wiki/Fixed-point_combinator
%% license: public domain
%% version: 1.0

-module(y).
-export([y/1]).

y(F) -> G = fun (M) -> F(fun(A) -> (M(M))(A) end) end, G(G).

%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

y_combinator_test() ->
	SumFun = fun(F) ->
			fun	({[], Acc}) ->
					Acc;
				({[Head|Tail], Acc}) ->
					F({Tail, Head + Acc});
				(Array) ->
					F({Array, 0})
			end
	end,
	?assertEqual(6, (y:y(SumFun))([1,2,3])).

-endif.
