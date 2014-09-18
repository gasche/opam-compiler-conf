define USAGE
You can already use this script directly
  bash /path/to/opam-compiler-conf.sh

You can "install" it in your PATH to make it usable from `opam` as
  opam compiler-conf ...

To do this, just use
  make BINDIR=~/.local/bin install
with BINDIR set to something in your PATH.
endef
export USAGE

BINDIR=~/.local/bin

SRC=opam-compiler-conf.sh
DEST=$(BINDIR)/opam-compiler-conf

all:
	@echo "$$USAGE"

install:
	cp $(SRC) $(DEST)
	chmod +x $(DEST)

uninstall:
	rm $(DEST)
