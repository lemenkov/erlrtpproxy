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

-define(RTPPROXY_OK, "0").

-define(RTPPROXY_ERR_SYNTAX,    "E1").
-define(RTPPROXY_ERR_SOFTWARE,  "E7").
-define(RTPPROXY_ERR_NOSESSION, "E8").

-define (MOD_LIST, [{mod_asymmetric, $A}, {mod_e, $E}, {mod_i, $I}, {mod_ipv6, $6}, {mod_symmetric, $S}, {mod_weak, $W}, {mod_z, $Z}]).

-define(CMD_V, cmd_v).
-define(CMD_VF,cmd_vf).
-define(CMD_U, cmd_u).
-define(CMD_L, cmd_l).
-define(CMD_D, cmd_d).
-define(CMD_R, cmd_r).
-define(CMD_P, cmd_p).
-define(CMD_S, cmd_s).
-define(CMD_C, cmd_c).
-define(CMD_Q, cmd_q).
-define(CMD_X, cmd_x).
-define(CMD_I, cmd_i).

-define(PRINT(X, Y), utils:print(?MODULE, "[~w]: " ++  X ++ "~n", [self()] ++ Y)).

-define(ERR(X, Y), utils:print(?MODULE, "[~w]: " ++  X ++ "~n", [self()] ++ Y)).
-define(INFO(X, Y), utils:print(?MODULE, "[~w]: " ++  X ++ "~n", [self()] ++ Y)).
-define(WARN(X, Y), utils:print(?MODULE, "[~w]: " ++  X ++ "~n", [self()] ++ Y)).

