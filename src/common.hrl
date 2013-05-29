-record(cmd, {
		cookie=null,
		origin=null,
		type=null,
		callid=null,
		mediaid=0,
		from=null,
		to=null,
		params=null,
		timestamp = null
	}).

-record(response, {
		cookie = null,
		origin = null,
		type,
		data
	}).

-record(origin, {type, pid, fd, ip, port}).
-record(party, {tag, addr=null, rtcpaddr=null, proto=udp}).

-define(CMD_V, message_v).
-define(CMD_VF,message_vf).
-define(CMD_U, message_u).
-define(CMD_L, message_l).
-define(CMD_D, message_d).
-define(CMD_P, message_p).
-define(CMD_S, message_s).
-define(CMD_C, message_c).
-define(CMD_R, message_r).
-define(CMD_Q, message_q).
-define(CMD_X, message_x).
-define(CMD_I, message_i).
