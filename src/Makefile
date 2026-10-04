.PHONY: all clean test_lexer

all:
	cd .. && dune build --root . ./src/main.exe

test_lexer:
	cd .. && dune exec --root . ./src/test_lexer.exe
	dot -Tsvg /tmp/dfa.dot -o /tmp/dfa.svg
	dot -Tsvg /tmp/nfa.dot -o /tmp/nfa.svg

clean:
	cd .. && dune clean --root .
