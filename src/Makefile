.PHONY: all clean test_lexer

all: config.ml
	cd .. && dune build --root . ./src/main.exe

test_lexer: config.ml
	cd .. && dune exec --root . ./src/test_lexer.exe
	dot -Tsvg /tmp/dfa.dot -o /tmp/dfa.svg
	dot -Tsvg /tmp/nfa.dot -o /tmp/nfa.svg

config.ml: ../configure ../opts.mk
	cd .. && ./configure ${CONF_OPTS}

clean:
	cd .. && dune clean --root .
	rm -f config.ml
