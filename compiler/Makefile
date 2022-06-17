.PHONY: main.native test clean

all: main.native

main.native:
	ocamlbuild -I src main.native

test: main.native
	./main.native tests/test.code tests/test.bf

clean:
	ocamlbuild -clean
	rm -rf test.bf a.out
