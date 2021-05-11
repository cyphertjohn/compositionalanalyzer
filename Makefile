SOURCE_DIR = src
OCB = ocamlbuild
OCB_FLAGS = -use-ocamlfind -tag 'thread' -tag 'cclib(-lstdc++)' -package 'z3' -package 'gmp' -I $(SOURCE_DIR)

top:
	$(OCB) $(OCB_FLAGS) abstract.top

clean:
	$(OCB) -clean
	rm abstract.top