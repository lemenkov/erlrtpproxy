-module(utils).
-export([print/2, print/3]).
-export([make/0]).

print (Name, Format) ->
	print (Name, Format, []).

print (Name, Format, Params) ->
%	io:format(Format, Params),
	syslog:send(Name, syslog:info(), io_lib:format(Format, Params)).

make() ->
	lists:map(fun(X) -> compile:file(X, [verbose, report_errors, report_warnings]) end, [rtpproxy, call, media, syslog, ser, utils, player

