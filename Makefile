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
	ocp-indent -i src/*.ml src/*.mli

install:
	dune install 2> /dev/null
	codesign -s - `which why3find`

# --------------------------------------------------------------------------
