#!/bin/sh
spawn-fcgi -f ./dist/build/redigo-service/redigo-service -s /tmp/redigo.socket -M 511 $@
