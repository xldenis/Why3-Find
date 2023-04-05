# --------------------------------------------------------------------------
# ---  Why3 Package Manager
# --------------------------------------------------------------------------

.PHONY: all build clean

all: build

clean:
	dune clean

build:
	dune build

test:
	dune test

lint:
	ocp-indent -i src/*.ml src/*.mli utils/*.ml utils/*.mli ppx/*.ml ppx/*.mli

headers:
	headache -h HEADER src/*.ml src/*.mli src/*.mll utils/*.ml utils/*.mli ppx/*.ml ppx/*.mli

install:
	dune install 2> /dev/null
	codesign -s - `which why3find`

# --------------------------------------------------------------------------
