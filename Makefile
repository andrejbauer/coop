DUNE=dune

default: coop.exe

.PHONY: doc clean coop.exe tests

### Compilation of OCaml files

coop.exe:
	$(DUNE) build src/coop.exe

# Cleaning up

clean:
	$(DUNE) clean

# Build the documentation

doc:
	ocamlbuild -docflags -charset,utf8  $(OCAMLBUILD_MENHIRFLAGS) $(OCAMLBUILD_FLAGS) coop.docdir/index.html


# "make test" to see if anything broke
test: default
	cd tests && sh ./test.sh

# "make test-validate" to see if anything broke
# and ask for validation of possibly broken things.
test-validate: default
	cd tests && sh ./test.sh -v
