SOURCE_DIR = src
OCB = ocamlbuild
OCB_FLAGS = -use-ocamlfind -tag 'thread' -tag 'cclib(-lstdc++)' -package 'z3' -package 'gmp' -I $(SOURCE_DIR)

native:
	$(OCB) $(OCB_FLAGS) analyzer.native

doc:
	$(OCB) $(OCB_FLAGS) analyzer.docdir/index.html

clean:
	$(OCB) -clean