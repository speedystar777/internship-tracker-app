.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

tracker:
	OCAMLRUNPARAM=b dune exec bin/main.exe
	
check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f tracker.zip
	zip -r tracker.zip . -x@exclude.lst

clean:
	dune clean
	rm -f tracker.zip

doc:
	dune build @doc
