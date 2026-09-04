all: ecomp

include opts.mk

.PHONY: ecomp

src/config.ml: configure opts.mk
	./configure ${CONF_OPTS}

ecomp: src/config.ml
	dune build --root . ./src/main.exe
	ln -sf _build/default/src/main.exe ecomp

clean:
	dune clean --root .
	rm -f src/config.ml grammar.html
	rm -f ecomp
	make -C tests clean

test: ecomp
	make -C tests
