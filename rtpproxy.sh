#!/bin/bash

erl +K true -name 'rtpproxy1@host1.example.com' -setcookie testcookie -s rtpproxy start "127.0.0.1" "22222"
