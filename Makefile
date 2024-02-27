
TESTS  = $(wildcard examples/*.cob)
ERRORS = $(wildcard errors/*.cob)
DEST   = padovani@pianeta.di.unito.it:public_html/Software/CobaltBlue/
NAME   = dist/build/CobaltBlue/CobaltBlue

all:
	cabal build
	ln -fs $(NAME) CobaltBlue

tutorial:
	make -C tutorial

dist:
	cabal sdist

sync:
	make -C html
	scp html/*.* $(DEST)
	scp dist/*.tar.gz $(DEST)
	scp tutorial/CobaltBlueTutorial.pdf $(DEST)

%.check_ok:
	@$(NAME) --log $(@:%.check_ok=%) || echo

check_examples:
	@echo "THESE TESTS MUST SUCCEED"
	@echo "========================"
	@for i in $(TESTS); do make $$i.check_ok; done

%.check:
	@$(NAME) --log $(@:%.check=%) || echo

check_errors:
	@echo "THESE TESTS MUST FAIL (NOT WITH PARSE ERRORS)"
	@echo "============================================="
	@for i in $(ERRORS); do make $$i.check; done

check: check_examples check_errors

.PHONY: dist clean check check_examples check_errors

clean:
	cabal clean
	rm -f CobaltBlue


