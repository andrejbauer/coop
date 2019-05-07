default: coop.native

OCAMLBUILD = ocamlbuild
OCAMLBUILD_FLAGS = -j 4 -cflags -w,+a-4-27-29-50,"-warn-error +a" -lib unix -use-ocamlfind -pkg menhirLib -pkg sedlex
OCAMLBUILD_MENHIRFLAGS = -use-menhir -menhir "menhir --explain"

default: coop.native

.PHONY: doc clean coop.byte coop.native coop.d.byte coop.p.native

### Compilation of OCaml files

coop.byte coop.native coop.d.byte coop.p.native:
	ocamlbuild $(OCAMLBUILD_MENHIRFLAGS) $(OCAMLBUILD_FLAGS) $@

# Cleaning up

clean:
	$(OCAMLBUILD) -clean

# Build the documentation

doc:
	ocamlbuild -docflag -keep-code $(OCAMLBUILD_MENHIRFLAGS) $(OCAMLBUILD_FLAGS) coop.docdir/index.html
