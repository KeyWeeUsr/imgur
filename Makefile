EMACS := emacs

.PHONY: all
all: byte-compile test

.PHONY: clean
clean:
	@-rm imgur*.elc 2>/dev/null
	@-rm *.ok 2>/dev/null

%.elc: %.el
	@-rm "$@" 2>/dev/null
	@$(EMACS) --batch --quick \
		--directory . \
		--load compile-setup \
		--eval '(byte-compile-file "$(subst .elc,.el,$@)")' \
		&& test -f "$@"

byte-compile: \
	imgur.elc \
	imgur-tests.elc

.PHONY: test
test: byte-compile main-tests

.PHONY: integration-tests
integration-tests: integration-tests

imgur-integration-tests.ok: imgur-integration-tests.elc
	$(EMACS) --batch --quick \
		--directory . \
		--load imgur-integration-tests.el \
		--funcall ert-run-tests-batch \
	&& touch imgur-integration-tests.ok
integration-tests: imgur-integration-tests.ok

imgur-tests.ok: \
	imgur.elc imgur-tests.elc
	$(EMACS) --batch --quick \
		--directory . \
		--load imgur-tests.el \
		--funcall ert-run-tests-batch \
	&& touch imgur-tests.ok
main-tests: imgur-tests.ok

Makefile.ok: Makefile
	@make -n all
	@docker run \
		--network=none \
		--volume "$(PWD)"/Makefile:/Makefile \
		backplane/checkmake /Makefile
lint-makefile: Makefile.ok

.PHONY: tag
tag:
	$(MAKE) all
	git add -f . && git stash
	@grep ";; Version:" imgur.el \
		| tee /dev/stderr | grep "$(TAG)"
	@git tag "$(TAG)" --sign
