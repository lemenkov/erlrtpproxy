{
    application, rtpproxy,
	    [
		{description, "ipPort rtpproxy application"},
		{vsn, "0.1"},
		{modules, [rtpproxy, rtpproxy_app, rtprpoxy_sup, call, media, player, rtcp, ser.erl]},
		{registered, [rtpproxy, rtpproxy_sup]},
		{applications, [kernel, stdlib, eradius, eradius_acc, erlsyslog]},
		{env,
			[
			]},
		{mod, {rtpproxy_app, []}}
	    ]
}.
