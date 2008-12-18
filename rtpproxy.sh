#!/bin/bash

echo "after starting erlang node, start rtpproxy and ser interface as following:"
echo " "
echo "1> rtpproxy:start([])."
echo "2> ser:start({{127,0,0,1},22222})."
echo " "
erl +W w +K true -name 'rtpproxy1@host1.example.com' -setcookie testcookie 
