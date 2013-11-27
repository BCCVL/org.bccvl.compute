#!/bin/bash
# stores PID and exit code of managed process in current working dir

"$@" >job.out 2>job.err &
PID=$!

echo "$PID" > job.pid

wait $PID
RET=$?

echo "$RET" > job.exit
