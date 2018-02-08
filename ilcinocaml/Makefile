.PHONY: clean

OCB_FLAGS = -use-menhir -menhir "menhir --explain" -libs unix -I src
OCB = ocamlbuild $(OCB_FLAGS)

BUILD = native
LANG = ilc

default:
	$(OCB) $(LANG).$(BUILD)

clean:
	$(OCB) -clean
