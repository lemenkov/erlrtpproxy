-define(RtpHosts, [
			{'rtpproxy0@192.168.0.1', {192,168,0,1}, {min_port, 10000}, {max_port, 10900}},
			{'rtpproxy1@192.168.0.1', {192,168,0,1}, {min_port, 11000}, {max_port, 11900}},
			{'rtpproxy2@192.168.0.1', {192,168,0,1}, {min_port, 12000}, {max_port, 12900}},
			{'rtpproxy3@192.168.0.1', {192,168,0,1}, {min_port, 13000}, {max_port, 13900}},
			{'rtpproxy4@192.168.0.1', {192,168,0,1}, {min_port, 14000}, {max_port, 14900}},
			{'rtpproxy5@192.168.0.1', {192,168,0,1}, {min_port, 15000}, {max_port, 15900}},
			{'rtpproxy6@192.168.0.1', {192,168,0,1}, {min_port, 16000}, {max_port, 16900}},
			{'rtpproxy7@192.168.0.1', {192,168,0,1}, {min_port, 17000}, {max_port, 17900}}
		]).

-define(PING_TIMEOUT, 100).

% We need at least 4 ports per each media (2 for RTP and 2 for RTCP)
-define(PORTS_PER_MEDIA, 4).

-define(MEDIA_TIME_TO_LIVE, 2000).
-define(CALL_TIME_TO_LIVE, 30000).

