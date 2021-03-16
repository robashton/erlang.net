NIFS=liberldotnet

erl_root_dir = $(shell erl -eval 'io:format(standard_error, "~s", [os:getenv("ROOTDIR")]), erlang:halt().' 2>&1 >/dev/null)

ifeq ($(_KERL_ACTIVE_DIR),)
	ERLDIR = $(call erl_root_dir)
else
	ERLDIR = $(_KERL_ACTIVE_DIR)
endif

CONFIGURATION=Debug

all: nifs priv/testimpl.dll priv/cslib.dll priv/cslib.runtimeconfig.json

priv/:
	mkdir -p priv/


c_src/%.o: c_src/%.cpp c_src/*.h c_src/*.cpp | priv/
	gcc -std=c++14 -c -I$(ERLDIR)/usr/include -I$(DOTNET_LOCATION) -o $@ $<

priv/liberldotnet.so: c_src/dotnet.o c_src/dotnet_exports.o c_src/utils.o
	gcc -o $@ $^ -shared -fPIC -L $(DOTNET_LOCATION) -L$(ERLDIR)/usr/lib -lstdc++ -lnethost -lei -ldl

priv/cslib.dll: cslib/bin/$(CONFIGURATION)/net5.0/cslib.dll
	cp $< $@

priv/testimpl.dll: testimpl/bin/$(CONFIGURATION)/net5.0/testimpl.dll
	cp $< $@

priv/cslib.runtimeconfig.json: cslib/bin/$(CONFIGURATION)/net5.0/cslib.runtimeconfig.json
	cp $< $@

cslib/bin/$(CONFIGURATION)/net5.0/cslib.runtimeconfig.json: cslib/bin/$(CONFIGURATION)/net5.0/cslib.dll

cslib/bin/$(CONFIGURATION)/net5.0/cslib.dll: cslib/*.cs cslib/*/*.cs cslib/cslib.csproj
	cd cslib && dotnet build

testimpl/bin/$(CONFIGURATION)/net5.0/testimpl.dll: testimpl/*.cs testimpl/*/*.cs testimpl/testimpl.csproj
	cd testimpl && dotnet build

nifs: $(addprefix priv/, $(addsuffix .so, $(NIFS)))


