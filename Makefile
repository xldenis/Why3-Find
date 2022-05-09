# --------------------------------------------------------------------------
# ---  Why3 Package Manager
# --------------------------------------------------------------------------

.PHONY: all build clean

all: build

clean:
	dune clean

build:
	dune build

install:
	dune install
	codesign -s - `which why3find`

# --------------------------------------------------------------------------
