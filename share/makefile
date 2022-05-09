# --------------------------------------------------------------------------
# ---  WHY3 Makefile
# --------------------------------------------------------------------------

#WHY3_PACKAGE= name
#WHY3_DEPENDS= pkg ...
#WHY3_EXTRACT= m.M ...
#WHY3_LIBRARIES= ocaml pkg ...
#WHY3_OPTIONS= OPTIONS...
#WHY3_HAMMER= OPTIONS...

# --------------------------------------------------------------------------

.PHONY: all

all:: prove

# --------------------------------------------------------------------------
# ---  Targets
# --------------------------------------------------------------------------

WHY3_TARGETS=$(shell find $(WHY3_PACKAGE) -name "*.mlw")

.PHONY: compile check replay fix

compile: $(patsubst %.mlw,%.cc,$(WHY3_TARGETS))

replay: $(patsubst %.mlw,%.replay,$(WHY3_TARGETS))
	@echo "-----------------------------"

check: $(patsubst %.mlw,%.check,$(WHY3_TARGETS))

prove: $(patsubst %.mlw,%.prove,$(WHY3_TARGETS))

fix: $(patsubst %.mlw,%.fix,$(WHY3_TARGETS))
	@echo "-----------------------------"

clean:: $(pathsubst %.mlw,%.cleandir,$(WHY3_TARGETS))

# --------------------------------------------------------------------------
# ---  Patterns
# --------------------------------------------------------------------------

WHY3_PKGS=$(addprefix "-p ",$(WHY3_DEPENDS))
WHY3_CC=why3find compile $(WHY3_OPTIONS) $(WHY3_PKGS)
WHY3_IDE=why3find ide $(WHY3_OPTIONS) $(WHY3_PKGS)
WHY3_REPLAY=why3find replay -f $(WHY3_OPTIONS) $(WHY3_PKGS)
WHY3_PROVE=why3find hammer -u $(WHY3_HAMMER) $(WHY3_PKGS)

%.dir: %.mlw
	@mkdir -p $*

%.cleandir: %.mlw
	@rm -fr $*

%.cc: %.mlw
	@echo "> why3find compile [...] $<"
	@$(WHY3_CCP) $<

%.chk: %.mlw
	@($(WHY3_CCP) $<)

%.ide: %.chk %.dir
	@echo "> why3find ide $*.mlw, waiting…"
	@($(WHY3_IDE) $*.mlw)

%.prove: %.chk %.dir
	@echo "> why3find hammer -u $*.mlw"
	@($(WHY3_PROVE) $*.mlw)

%.fix: %.chk %.dir
	@echo "-----------------------------"
	@echo "--- fixup $*"
	@echo "-----------------------------"
	@$(WHY3_PROVE) $*.mlw | tee .log
	@if grep -q "\(failed\|not proved\)" .log ;\
	 then echo "[Fix Required] why3 ide $*, waiting…"; $(WHY3_IDE) $*.mlw; fi

%.check: %.chk %.dir
	@echo "--- check $*"
	@$(WHY3_REPLAY) --obsolete-only $* > .log 2> /dev/null
	@if grep -q "\(failed\|not proved\)" .log ;\
	 then cat .log ; exit 1;\
	 fi

%.replay: %.chk %.dir
	@echo "-----------------------------"
	@echo "--- replay $*"
	@echo "-----------------------------"
	@$(WHY3_REPLAY) $* | tee .log
	@if grep -q "\(failed\|not proved\)" .log ; then exit 1; f

# --------------------------------------------------------------------------
# ---  Extraction
# --------------------------------------------------------------------------

.PHONY: extract

WHY3_LIBS=$(shell why3find query -l $(WHY3_DEPENDS))

lib/dune: Makefile
	@mkdir -p lib
	@echo "(library" > $@
	@echo "  (name $(WHY3_PACKAGE))" >> $@
	@echo "  (public_name why3lib-$(WHY3_PACKAGE))" >> $@
	@echo "  (wrapped false)" >> $@
	@echo "  (libraries $(WHY3_LIBRARIES) $(WHY3_LIBS)))" >> $@
	@echo "> generated ./lib/dune"

extract: lib/dune
	@echo "> why3find extract -d ./lib [...]"
	@rm -f lib/*__*.ml
	@mkdir -p lib
	@why3find extract --driver ocaml64 --modular -d ./lib \
		$(WHY3_OPTIONS) $(WHY3_PKGS) $(WHY3_EXTRACT)

# --------------------------------------------------------------------------
# ---  INSTALL
# --------------------------------------------------------------------------

.PHONY: install uninstall

install::
	why3find install $(WHY3_PACKAGE) \
		$(WHY3_DEPENDS) \
		$(WHY3_TARGETS)

uninstall::
	whyfind uninstall $(WHY3_PACKAGE)

# --------------------------------------------------------------------------
