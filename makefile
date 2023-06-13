all: counter people config run

counter: counter.ml
	ocamlbuild -use-ocamlfind counter.byte

people: people.ml
	ocamlbuild -use-ocamlfind people.byte

config: config.ml
	ocamlbuild -use-ocamlfind config.byte

run: run.ml
	ocamlbuild -use-ocamlfind run.byte

clean:
	rm -rf _build *.byte

