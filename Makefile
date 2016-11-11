wget  ?= wget
RM     = rm -rf
emacs ?= emacs

.PHONY: test clean distclean nvp
all: test
test: nvp
	$(emacs) -Q -batch --eval '(progn (push "." load-path))' \
	-L . -l ert -l test/c-tests.el                      \
	-f ert-run-tests-batch-and-exit
	$(RM) nvp

.INTERMEDIATE: nvp
nvp:
	if [ ! -d nvp ]; then                                    \
	  git clone "https://github.com/nverno/nvp";             \
	fi

README.md: el2markdown.el c-tools.el
	$(emacs) -batch -l $< c-tools.el -f el2markdown-write-readme
	$(RM) $@~

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~

distclean: clean
	$(RM) *autoloads.el *loaddefs.el TAGS *.elc
