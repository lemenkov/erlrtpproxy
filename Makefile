all:
	erlc call.erl media.erl rtpproxy.erl ser.erl utils.erl player.erl rtcp.erl

clean:
	rm -f *.beam *~
