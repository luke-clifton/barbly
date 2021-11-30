#! /usr/bin/env bash

jq -n '
 { title: now | todate
 , items:
   [ { label: "re-run \(env.LOL)", exec: ["bash", "-c", "echo 1 >> /dev/fd/$1", "bash", env.LOL] }
   ]
 }'
