{
    application, rtpproxy,
	    [
		{description, "ipPort rtpproxy application"},
		{vsn, "0.1"},
		{modules, [rtpproxy, rtpproxy_app, rtprpoxy_sup, call, media, player, rtcp]},
		{registered, [rtpproxy, rtpproxy_sup]},
		{applications, [kernel, stdlib, erlsyslog]},
		{env,
			[
				{ping_timeout, 100},
				{ports_per_media, 4},
				{radacct_servers, [[{127,0,0,1},1813,"testradacctpass"]]},
				{rtphosts, [
						{'rtpproxy1@example.com', {192,168,1,2}, {min_port, 10000}, {max_port, 10900}},
						{'rtpproxy2@example.com', {192,168,1,2}, {min_port, 11000}, {max_port, 11900}},
						{'rtpproxy3@example.com', {192,168,1,2}, {min_port, 12000}, {max_port, 12900}},
						{'rtpproxy4@example.com', {192,168,1,2}, {min_port, 13000}, {max_port, 13900}},
						{'rtpproxy5@example.com', {192,168,1,2}, {min_port, 14000}, {max_port, 14900}},
						{'rtpproxy6@example.com', {192,168,1,2}, {min_port, 15000}, {max_port, 15900}},
						{'rtpproxy7@example.com', {192,168,1,2}, {min_port, 16000}, {max_port, 17000}},
						{'rtpproxy8@example.com', {192,168,1,2}, {min_port, 17000}, {max_port, 17900}}
					]},
				{syslog_address, {"localhost", 514}},
				{sources, [call, media, player, rtcp, rtpproxy, ser, y]}
			]},
		{mod, {rtpproxy_app, []}}
	    ]
}.
