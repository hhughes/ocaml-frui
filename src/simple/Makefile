JS=simple.js

all: myocamlbuild.ml
	ocamlbuild $(JS); ln -s _build/$(JS) .

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../../ocamljs/tools/myocamlbuild.ml
