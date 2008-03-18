#!/bin/bash

erl +K true -name rtpproxy1 -s rtpproxy start "rtpproxy1@host1.example.com" "127.0.0.1" "22222"
