all:
	@echo "Nothing to compile, use make install"

FILES= init.sh \
	gitignore.template \
	Makefile.template \
	Makefile.why3 \
	hammer.cfg \
	admit.mlw

install:
	@echo ocamlfind install why3-make ...
	@ocamlfind remove why3-make 2> /dev/null
	@ocamlfind install why3-make META $(FILES)
	@echo "---------------------------------------------------------------"
	@ocamlfind query -l why3-make | grep .
	@echo "---------------------------------------------------------------"
