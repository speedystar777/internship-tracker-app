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
	rm -f _coverage/

doc:
	dune build @doc

coverage: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage