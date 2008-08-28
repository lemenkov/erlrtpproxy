all:
	erlc call.erl media.erl rtpproxy.erl ser.erl syslog.erl utils.erl player.erl

clean:
	rm -f *.beam *~
