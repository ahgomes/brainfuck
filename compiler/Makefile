.PHONY: test interp clean
.SILENT: main.native test interp

all: main.native

main.native:
	ocamlbuild -use-menhir -I src main.native

test: main.native
	./main.native test/test.code test/test.bf

interp: main.native test
	ocaml ../interp.ml test/test.bf

clean:
	ocamlbuild -clean
	rm -rf test.bf a.out
