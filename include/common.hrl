-record(cmd, {
		cookie=null,
		origin=null,
		type=null,
		callid=null,
		mediaid=0,
		from=null,
		to=null,
		params=null
	}).

-record(origin, {type, pid, ip, port}).
-record(party, {tag, addr=null, rtcpaddr=null, proto=udp}).

-define(CMD_U, message_u).
-define(CMD_D, message_d).
-define(CMD_P, message_p).
-define(CMD_S, message_s).
-define(CMD_C, message_c).
-define(CMD_Q, message_q).
-define(CMD_X, message_x).
-define(CMD_I, message_i).

-include_lib("erlsyslog/include/erlsyslog.hrl").

