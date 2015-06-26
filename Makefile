PROJECT=estuary
REBAR=bin/rebar
RELX=bin/relx

all: get-deps compile

build-plt:
	@dialyzer --build_plt --output_plt ~/.$(PROJECT).plt \
		--apps kernel stdlib

check-plt:
	@dialyzer --check_plt --plt ~/.$(PROJECT).plt

clean:
	@( $(REBAR) clean )

compile:
	@( $(REBAR) compile )

dialyze:
	@dialyzer ebin/*.beam --plt ~/.$(PROJECT).plt -I include

dist:
	@( $(RELX) )

doc:
	@echo "Running rebar doc..."
	@$(REBAR) skip_deps=true doc

eunit:
	@echo "Running rebar eunit..."
	@$(REBAR) skip_deps=true eunit

get-deps:
	@( $(REBAR) get-deps )

run:
	@( erl +W w -pa ebin deps/*/ebin -config rel/sys.config -sname estuary -sync log all -s estuary )

test: all eunit

.PHONY: dialyze doc eunit
