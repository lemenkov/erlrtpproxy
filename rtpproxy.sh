#!/bin/bash

#cd /root/rtpproxy && erl +K true -name rtpproxy -noshell -s rtpproxy start_link "127.0.0.1" "11111"
cd /root/rtpproxy && erl +K true -noshell -s rtpproxy start "127.0.0.1" "11111"
#cd /root/rtpproxy && erl +K true -noshell -s rtpproxy start "127.0.0.1" "22222"

