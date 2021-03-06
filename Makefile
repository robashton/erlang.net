NIFS=dotnet

erl_root_dir = $(shell erl -eval 'io:format(standard_error, "~s", [os:getenv("ROOTDIR")]), erlang:halt().' 2>&1 >/dev/null)

ifeq ($(_KERL_ACTIVE_DIR),)
	ERLDIR = $(call erl_root_dir)
else
	ERLDIR = $(_KERL_ACTIVE_DIR)
endif

CONFIGURATION=Debug

all: nifs priv/cslib.dll priv/cslib.runtimeconfig.json

priv/:
	mkdir -p priv/


c_src/%.o: c_src/%.cpp | priv/
	gcc -std=c++14 -c -I$(ERLDIR)/usr/include -I$(DOTNET_LOCATION) -o $@ $<

priv/%.so: c_src/%.o 
	gcc -o $@ $< -shared -fPIC -L $(DOTNET_LOCATION) -L$(ERLDIR)/usr/lib -lstdc++ -lnethost -lei -ldl 

priv/cslib.dll: cslib/bin/$(CONFIGURATION)/net5.0/cslib.dll
	cp $< $@

priv/cslib.runtimeconfig.json: cslib/bin/$(CONFIGURATION)/net5.0/cslib.runtimeconfig.json
	cp $< $@

cslib/bin/$(CONFIGURATION)/net5.0/cslib.runtimeconfig.json: cslib/bin/$(CONFIGURATION)/net5.0/cslib.dll

cslib/bin/$(CONFIGURATION)/net5.0/cslib.dll: cslib/*.cs cslib/cslib.csproj
	cd cslib && dotnet build

nifs: $(addprefix priv/, $(addsuffix .so, $(NIFS)))


