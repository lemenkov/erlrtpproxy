-module(utils).
-export([print/3, print/4]).
-export([y/1]).
-export([make/0]).

print (Name, Level, Format) ->
	print (Name, Level, Format, []).

print (Name, Level, Format, Params) when is_list (Params) ->
	io:format(Format, Params),
%	syslog:send(Name, Level, io_lib:format(Format, Params)).
	ok.

y(M) ->
	G = fun (F) -> M(fun(A) -> (F(F))(A) end) end,
	G(G).

make() ->
	lists:foreach(fun(X) -> compile:file(X, [verbose, report_errors, report_warnings]) end, [rtpproxy, call, media, ser, utils, player]).

