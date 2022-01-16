#! /usr/bin/env bash
set -xe

rm -rf output/*
mkdir -p output

for cmd in "lwt_unix_server" "async_server" "lwt_unix_server_new"; do
  ./$cmd.exe &
  running_pid=$!
  echo "Measuring latency of $cmd"
  sleep 2;
  wrk2 \
    -t2 -c1000 -d5s \
    --timeout 2000 \
    -R 80000 --latency \
    -H 'Connection: keep-alive' \
    "http://localhost:8080" > output/run-$cmd.txt;
  kill ${running_pid};
  sleep 1;
done
echo "The results are available in $PWD/output"
