SBCL ?= lisp
ECL ?= ecl
PACKAGE := simple-asteroids

.PHONY: all clean test run build-sbcl

all: clean build

run: | $(PACKAGE)
	./$(firstword $|)

build-sbcl: clean
	$(SBCL) --eval '(ql:quickload :$(PACKAGE))' \
			--eval "(sb-ext:save-lisp-and-die \
					 \"$(PACKAGE)\" :toplevel #'$(PACKAGE):main \
					 :executable t :purify t :compression 9)"

clean:
	-rm $(PACKAGE)

test:
	echo dont know how to test yet
