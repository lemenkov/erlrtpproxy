-define(RtpHosts, [	{'rtpproxy@omega1.ipport.net', {89,208,190,62}, {min_port, 10000}, {max_port, 65000}}
		]).
%{'rtpproxy@213.248.12.116',    '213.248.12.116'}

-define(PING_TIMEOUT, 100).

% We need at least 4 ports per each media (2 for RTP and 2 for RTCP)
-define(PORTS_PER_MEDIA, 4).

-define(MEDIA_TIME_TO_LIVE, 2000).

