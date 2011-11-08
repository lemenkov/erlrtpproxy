%%%----------------------------------------------------------------------
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ser_utils).

-export([is_rfc1918/1]).

% TODO only IPv4 for now
is_rfc1918({I0,I1,I2,I3} = IPv4) when
	is_integer(I0), I0 >= 0, I0 < 256,
	is_integer(I1), I1 >= 0, I1 < 256,
	is_integer(I2), I2 >= 0, I2 < 256,
	is_integer(I3), I3 >= 0, I3 < 256 ->
	is_rfc1918_guarded(IPv4);
is_rfc1918({I0, I1, I2, I3, I4, I5, I6, I7} = _IPv6) when
	is_integer(I0), I0 >= 0, I0 < 65535,
	is_integer(I1), I1 >= 0, I1 < 65535,
	is_integer(I2), I2 >= 0, I2 < 65535,
	is_integer(I3), I3 >= 0, I3 < 65535,
	is_integer(I4), I4 >= 0, I4 < 65535,
	is_integer(I5), I5 >= 0, I5 < 65535,
	is_integer(I6), I6 >= 0, I6 < 65535,
	is_integer(I7), I7 >= 0, I7 < 65535 ->
		ipv6;
is_rfc1918(_) ->
	throw({error, "Not a valid IP address"}).

% Loopback (actually, it's not a RFC1918 network)
is_rfc1918_guarded({127,_,_,_}) ->
	true;
% RFC 1918, 10.0.0.0 - 10.255.255.255
is_rfc1918_guarded({10,_,_,_}) ->
	true;
% RFC 1918, 172.16.0.0 - 172.31.255.255
is_rfc1918_guarded({172,I1,_,_}) when I1 > 15, I1 < 32 ->
	true;
% RFC 1918, 192.168.0.0 - 192.168.255.255
is_rfc1918_guarded({192,168,_,_}) ->
	true;
is_rfc1918_guarded(_) ->
	false.
