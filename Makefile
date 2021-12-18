BIN ?= $(shell pwd)/bin

LUA_SRC ?= $(shell pwd)/lua-5.4.3
FENNEL_SRC ?= $(shell pwd)/fennel

LUA ?= $(BIN)/lua
FENNEL ?= $(BIN)/fennel

all: compiler

compiler: $(LUA) $(FENNEL)

$(LUA): | $(BIN)
	$(MAKE) -C $(LUA_SRC)
	ln -sf "$(LUA_SRC)/src/lua" $(LUA)

$(FENNEL): $(LUA) | $(BIN)
	$(MAKE) -C "$(FENNEL_SRC)" "LUA=$(LUA)" "LUA_DIR=$(LUA_SRC)"
	ln -sf "$(FENNEL_SRC)/fennel" $(FENNEL)

$(BIN):
	mkdir -p $(BIN)

clean:
	$(MAKE) -C $(FENNEL_SRC) clean "LUA_DIR=$(LUA_SRC)"
	rm -rf $(BIN)

.PHONY: all compiler clean
