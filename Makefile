
PKGS = yojson
OCB_FLAGS = -tag bin_annot -pkgs $(PKGS) -use-ocamlfind -r
OCB = 		ocamlbuild $(OCB_FLAGS)
ARCH=unix

all: native # byte profile debug

js: ARCH=js
js: byte
	js_of_ocaml main.byte


clean:
	$(OCB) -I platform/$(ARCH) -clean

native:
	$(OCB) -I platform/$(ARCH) main.native

byte:
	$(OCB) -I platform/$(ARCH) main.byte

profile:
	$(OCB) -I platform/$(ARCH) -tag profile main.native

debug:
	$(OCB) -I platform/$(ARCH) -tag debug main.byte
