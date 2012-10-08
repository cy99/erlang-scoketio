#!/bin/sh
erl -sname socketio -pa ebin -pa deps/*/ebin -s socketio \
	-eval "io:format(\"Success!~n\")."