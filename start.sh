#!/bin/sh
cd `dirname $0`
erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -sname ocl_node -boot cl_example $1
