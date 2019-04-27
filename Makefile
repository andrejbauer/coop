default: terminus.native

OCAMLBUILD = ocamlbuild
OCAMLBUILD_FLAGS = -j 4 -cflags -w,+a-4-27-29-50,"-warn-error +a" -lib unix -use-ocamlfind -pkg menhirLib -pkg sedlex
OCAMLBUILD_MENHIRFLAGS = -use-menhir -menhir "menhir --explain"

default: terminus.native

.PHONY: doc clean terminus.byte terminus.native terminus.d.byte terminus.p.native

### Compilation of OCaml files

terminus.byte terminus.native terminus.d.byte terminus.p.native:
	ocamlbuild $(OCAMLBUILD_MENHIRFLAGS) $(OCAMLBUILD_FLAGS) $@

# Cleaning up

clean:
	$(OCAMLBUILD) -clean

# Build the documentation

doc:
	ocamlbuild -docflag -keep-code $(OCAMLBUILD_MENHIRFLAGS) $(OCAMLBUILD_FLAGS) terminus.docdir/index.html
