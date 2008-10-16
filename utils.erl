-module(utils).
-export([print/2, print/3]).
-export([y/1]).
-export([make/0]).

print (Name, Format) ->
	print (Name, Format, []).

print (Name, Format, Params) when is_list (Params) ->
%	io:format(Format, Params),
	syslog:send(Name, syslog:info(), io_lib:format(Format, Params)).
%	ok.

y(M) ->
	G = fun (F) -> M(fun(A) -> (F(F))(A) end) end,
	G(G).

make() ->
	lists:foreach(fun(X) -> compile:file(X, [verbose, report_errors, report_warnings]) end, [rtpproxy, call, media, syslog, ser, utils, player]).

