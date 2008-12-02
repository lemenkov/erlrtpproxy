-module(utils).
-export([print/3, print/4]).
-export([y/1]).

print (Name, Level, Format) ->
	print (Name, Level, Format, []).

print (Name, Level, Format, Params) when is_list (Params) ->
	io:format(Format, Params),
%	syslog:send(Name, Level, io_lib:format(Format, Params)).
	ok.

y(M) ->
	G = fun (F) -> M(fun(A) -> (F(F))(A) end) end,
	G(G).

