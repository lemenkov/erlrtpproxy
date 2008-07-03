-record(cmd, {
		cookie=null,
		type=null,
		callid=null,
		addr=null,
		from=null,
		to=null,
		filename=null,
		codecs=null,
		params=null
	}).

-define(RTPPROXY_OK, "0").

-define(RTPPROXY_ERR_SYNTAX,    "E1").
-define(RTPPROXY_ERR_SOFTWARE,  "E7").
-define(RTPPROXY_ERR_NOSESSION, "E8").

-define (MOD_LIST, [{mod_asymmetric, $A}, {mod_e, $E}, {mod_i, $I}, {mod_ipv6, $6}, {mod_symmetric, $S}, {mod_weak, $W}, {mod_z, $Z}]).

-define(CMD_V, 0).
-define(CMD_VF,1).
-define(CMD_U, 2).
-define(CMD_L, 3).
-define(CMD_D, 5).
-define(CMD_R, 6).
-define(CMD_P, 7).
-define(CMD_S, 8).
-define(CMD_C, 9).
-define(CMD_Q, 10).
-define(CMD_X, 11).
-define(CMD_I, 12).

