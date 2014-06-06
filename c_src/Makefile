JULIA_DIR=/home/edtate/julia
ERLANG_DIR=/usr/local/lib/erlang/
ERLANG_INCLUDE_PATH=/usr/local/lib/erlang/usr/include/

compile:
	gcc -o jl_nif.so -shared jl_nif.c -I$(JULIA_DIR)/src -I$(JULIA_DIR)/src/support -I$(JULIA_DIR)/usr/include -I$(ERLANG_DIR)/usr/include -L$(JULIA_DIR)/usr/lib -fPIC -ljulia
