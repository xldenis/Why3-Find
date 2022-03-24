all:
	@echo "Nothing to compile, use make install."

FILES= init.sh \
	gitignore.template \
	Makefile.template \
	opam.template \
	Makefile.why3 \
	hammer.cfg \
	admit.mlw

install:
	@ocamlfind remove why3-make 2> /dev/null
	@ocamlfind install why3-make META $(FILES)
