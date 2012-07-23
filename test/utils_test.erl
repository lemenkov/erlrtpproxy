%%%----------------------------------------------------------------------
%%% Copyright (c) 2012 Peter Lemenkov <lemenkov@gmail.com>
%%%
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification,
%%% are permitted provided that the following conditions are met:
%%%
%%% * Redistributions of source code must retain the above copyright notice, this
%%% list of conditions and the following disclaimer.
%%% * Redistributions in binary form must reproduce the above copyright notice,
%%% this list of conditions and the following disclaimer in the documentation
%%% and/or other materials provided with the distribution.
%%% * Neither the name of the authors nor the names of its contributors
%%% may be used to endorse or promote products derived from this software
%%% without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ''AS IS'' AND ANY
%%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
%%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
