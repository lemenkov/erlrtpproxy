-record(cmd, {
		cookie=null,
		origin=null,
		type=null,
		callid=null,
		addr=null,
		from=null,
		to=null,
		filename=null,
		codecs=null,
		params=null
	}).

-record(origin, {type, pid, ip, port}).

-define(RTPPROXY_OK, "0").

-define(RTPPROXY_ERR_SYNTAX,    "E1").
-define(RTPPROXY_ERR_SOFTWARE,  "E7").
-define(RTPPROXY_ERR_NOSESSION, "E8").

-define (MOD_LIST, [{mod_asymmetric, $A}, {mod_e, $E}, {mod_i, $I}, {mod_ipv6, $6}, {mod_symmetric, $S}, {mod_weak, $W}, {mod_z, $Z}]).

-define(CMD_V, message_v).
-define(CMD_VF,message_vf).
-define(CMD_U, message_u).
-define(CMD_L, message_l).
-define(CMD_D, message_d).
-define(CMD_R, message_r).
-define(CMD_P, message_p).
-define(CMD_S, message_s).
-define(CMD_C, message_c).
-define(CMD_Q, message_q).
-define(CMD_X, message_x).
-define(CMD_I, message_i).

-define(ERR(X, Y),  utils:print(?MODULE, syslog:error(),   "[~w]: " ++  X ++ "~n", [self()] ++ Y)).
-define(INFO(X, Y), utils:print(?MODULE, syslog:info(),    "[~w]: " ++  X ++ "~n", [self()] ++ Y)).
-define(WARN(X, Y), utils:print(?MODULE, syslog:warning(), "[~w]: " ++  X ++ "~n", [self()] ++ Y)).

-define(SOURCES, [call, media, player, rtcp, rtpproxy, ser, utils]).

