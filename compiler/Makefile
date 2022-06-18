.PHONY: main.native test clean

all: main.native

main.native:
	ocamlbuild -use-menhir -I src main.native

test: main.native
	./main.native test/test.code test/test.bf

clean:
	ocamlbuild -clean
	rm -rf test.bf a.out
