-define(RtpHosts, [	{'rtpproxy@host1.example.com', {192,168,0,100}, {min_port, 35000}, {max_port, 65000}},
			{'rtpproxy@host2.example.com', {192,168,0,101}, {min_port, 35000}, {max_port, 65000}}
		]).

-define(PING_TIMEOUT, 100).

% We need at least 4 ports per each media (2 for RTP and 2 for RTCP)
-define(PORTS_PER_MEDIA, 4).
