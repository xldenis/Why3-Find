# --------------------------------------------------------------------------
# ---  Why3 Package Manager
# --------------------------------------------------------------------------

.PHONY: all build clean test lint headers install uninstall

all: build

clean:
	dune clean

build:
	dune build

test:
	@rm -f why3find.json
	dune test

lint:
	ocp-indent -i src/*.ml src/*.mli utils/*.ml utils/*.mli

headers:
	headache -h HEADER src/*.ml src/*.mli src/*.mll utils/*.ml utils/*.mli

install:
	dune install 2> /dev/null

uninstall:
	dune uninstall 2> /dev/null

# --------------------------------------------------------------------------
