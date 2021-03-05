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

priv/%.so: c_src/%.c | priv/
	$(CC) -shared -fPIC -lei -I$(ERLDIR)/usr/include -L$(ERLDIR)/usr/lib -o $@ $<

nifs: $(addprefix priv/, $(addsuffix .so, $(NIFS)))
