-module(rtpsocket).

%-behavior(gen_fsm).
%-export([start_link/1]).
%-export([send_event/2]).

-export([watcher/1]).


watcher (Parent) ->
	receive
		{udp, Fd, Ip, Port, Msg} ->
			gen_server:cast(Parent, {udp, {Fd, Ip, Port, Msg}}),
			watcher(Parent);
		{Parent, destroy} ->
			ok;
		_Other ->
			watcher(Parent)
	after 60000 ->
%	after 3000 ->
		gen_server:cast(Parent, {self(), timeout})
	end.

