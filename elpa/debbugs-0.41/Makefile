EMACS ?= emacs

SOURCE=$(wildcard *.el)
TESTSOURCE=$(wildcard test/*.el)
TARGET=$(filter-out debbugs-pkg.elc,$(patsubst %.el,%.elc,$(SOURCE)))
TESTTARGET=$(patsubst %.el,%.elc,$(TESTSOURCE))


.PHONY: all build check clean
.PRECIOUS: %.elc

%.elc: %.el
	@$(EMACS) -Q -batch -L . -f batch-byte-compile $<

all: build

build: $(TARGET)

check: build $(TESTTARGET)
	@$(EMACS) -Q --batch -L . -l $(TESTSOURCE) -f ert-run-tests-batch-and-exit

clean:
	-rm -f $(TARGET) $(TESTTARGET)
