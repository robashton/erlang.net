NIFS=liberldotnet

erl_root_dir = $(shell erl -eval 'io:format(standard_error, "~s", [os:getenv("ROOTDIR")]), erlang:halt().' 2>&1 >/dev/null)

ifeq ($(_KERL_ACTIVE_DIR),)
	ERLDIR = $(call erl_root_dir)
else
	ERLDIR = $(_KERL_ACTIVE_DIR)
endif

CONFIGURATION=Release

all: nifs priv/testimpl.dll priv/Erlang.dll priv/Erlang.runtimeconfig.json

priv/:
	mkdir -p priv/


c_src/%.o: c_src/%.cpp c_src/*.h c_src/*.cpp | priv/
	gcc -std=c++14 -c -I$(ERLDIR)/usr/include -I$(DOTNET_LOCATION) -o $@ $<

priv/liberldotnet.so: c_src/dotnet.o c_src/dotnet_exports.o c_src/utils.o
	gcc -o $@ $^ -shared -fPIC -L $(DOTNET_LOCATION) -L$(ERLDIR)/usr/lib -lstdc++ -lnethost -lei -ldl

priv/Erlang.dll: cslib/bin/$(CONFIGURATION)/net5.0/Erlang.dll
	cp $< $@

priv/testimpl.dll: testimpl/bin/$(CONFIGURATION)/net5.0/testimpl.dll
	cp $< $@

priv/Erlang.runtimeconfig.json: cslib/bin/$(CONFIGURATION)/net5.0/Erlang.runtimeconfig.json
	cp $< $@

Erlang/bin/$(CONFIGURATION)/net5.0/Erlang.runtimeconfig.json: Erlang/bin/$(CONFIGURATION)/net5.0/Erlang.dll

Erlang/bin/$(CONFIGURATION)/net5.0/Erlang.dll: cslib/*.cs cslib/*/*.cs cslib/Erlang.csproj
	cd cslib && dotnet build -c $(CONFIGURATION)

testimpl/bin/$(CONFIGURATION)/net5.0/testimpl.dll: testimpl/*.cs testimpl/*/*.cs testimpl/testimpl.csproj
	cd testimpl && dotnet build -c $(CONFIGURATION)

nifs: $(addprefix priv/, $(addsuffix .so, $(NIFS)))


