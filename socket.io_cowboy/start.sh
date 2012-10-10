#!/bin/sh
erl -sname socketio -pa ebin -pa deps/*/ebin -s socketio -config app \
	-eval "io:format(\"Success!~n\")."