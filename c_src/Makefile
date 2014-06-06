all:
	gcc -o jl_nif.so -shared jl_nif.c -I$(JULIA_HOME)/src -I$(JULIA_HOME)/src/support -I$(JULIA_HOME)/usr/include -I$(ERLANG_HOME)/usr/include -L$(JULIA_HOME)/usr/lib -fPIC -ljulia
	mv jl_nif.so ../priv/
