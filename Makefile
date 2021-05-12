SOURCE_DIR = src
OCB = ocamlbuild
OCB_FLAGS = -use-ocamlfind -tag 'thread' -tag 'cclib(-lstdc++)' -package 'z3' -package 'gmp' -I $(SOURCE_DIR)

native:
	$(OCB) $(OCB_FLAGS) analyzer.native

top:
	$(OCB) $(OCB_FLAGS) analyzer.top

clean:
	$(OCB) -clean
	rm abstract.top