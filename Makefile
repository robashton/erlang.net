NIFS=dotnet

erl_root_dir = $(shell erl -eval 'io:format(standard_error, "~s", [os:getenv("ROOTDIR")]), erlang:halt().' 2>&1 >/dev/null)

ifeq ($(_KERL_ACTIVE_DIR),)
	ERLDIR = $(call erl_root_dir)
else
	ERLDIR = $(_KERL_ACTIVE_DIR)
endif

all: nifs

priv/:
	mkdir -p priv/


c_src/%.o: c_src/%.cpp | priv/
	gcc -std=c++14 -c -I$(ERLDIR)/usr/include -I$(DOTNET_LOCATION) -o $@ $<

priv/%.so: c_src/%.o 
	gcc -shared -Wl,--no-as-needed -fPIC -L $(DOTNET_LOCATION) -L$(ERLDIR)/usr/lib -lstdc++ -lnethost -lei -ldl -o $@ $<

nifs: $(addprefix priv/, $(addsuffix .so, $(NIFS)))
