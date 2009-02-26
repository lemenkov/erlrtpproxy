all:
	erlc -o ebin src/call.erl
	erlc -o ebin src/media.erl
	erlc -o ebin src/player.erl
	erlc -o ebin src/rtcp.erl
	erlc -o ebin src/rtpproxy.erl
	erlc -o ebin src/rtpproxy_app.erl
	erlc -o ebin src/rtpproxy_sup.erl
	erlc -o ebin src/ser.erl
	erlc -o ebin src/ser_app.erl
	erlc -o ebin src/ser_sup.erl

clean:
	rm -f ebin/*.beam priv/*~ src/*~ *~
