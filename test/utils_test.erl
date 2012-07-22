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

-module(utils_test).

%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


is_rfc1918_test_() ->
	[
		{"test for localhost",
			fun() -> ?assertEqual(true, utils:is_rfc1918({127,1,2,3})) end
		},
		{"test for 10.x.x.x subnet",
			fun() -> ?assertEqual(true, utils:is_rfc1918({10,0,127,3})) end
		},
		{"test for 172.x.x.x subnet",
			fun() -> ?assertEqual(true, utils:is_rfc1918({172,16,127,3})) end
		},
		{"test for 192.168.x.x subnet",
			fun() -> ?assertEqual(true, utils:is_rfc1918({192,168,127,3})) end
		},
		{"test #1 for non-RFC1918 subnet",
			fun() -> ?assertEqual(false, utils:is_rfc1918({172,168,127,3})) end
		},
		{"test #2 for non-RFC1918 subnet",
			fun() -> ?assertEqual(false, utils:is_rfc1918({192,169,127,3})) end
		},
		{"test for IPv6 subnet",
			fun() -> ?assertEqual(ipv6, utils:is_rfc1918({0,0,0,0,0,0,0,1})) end
		},
		{"test for invalid IP address",
			fun() -> ?assertThrow({error, "Not a valid IP address"}, utils:is_rfc1918({1001,1001,1001,1001})) end
		}
	].

-endif.
