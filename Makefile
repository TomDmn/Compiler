all: ecomp ecomp-run ecomp-report

TESTS_DIR ?= ../ecomp-tests
TEST_OUTPUT_DIR ?= $(abspath .test-results)
SUITE ?=

.PHONY: all ecomp ecomp-run ecomp-report completions check-completions deps clean test check-tests

check-tests:
	@if [ ! -f "$(TESTS_DIR)/Makefile" ]; then \
		echo "Error: the ecomp-tests repository could not be found." >&2; \
		echo "Looked for: $(abspath $(TESTS_DIR))/Makefile" >&2; \
		echo "Set its location explicitly, for example:" >&2; \
		echo "  make test TESTS_DIR=/path/to/ecomp-tests" >&2; \
		exit 2; \
	fi

ecomp:
	dune build --root . ./src/main.exe
	ln -sf _build/default/src/main.exe ecomp

ecomp-run:
	dune build --root . ./src/runner.exe
	ln -sf _build/default/src/runner.exe ecomp-run

ecomp-report:
	dune build --root . ./src/report_main.exe
	ln -sf _build/default/src/report_main.exe ecomp-report

completions:
	./tools/generate_completions.sh

check-completions:
	@tmp=$$(mktemp -d); \
	trap 'rm -rf "$$tmp"' EXIT; \
	cp -R completions "$$tmp/expected"; \
	./tools/generate_completions.sh; \
	diff -ru "$$tmp/expected" completions

deps:
	python3 tools/dune_deps.py --svg _build/ecomp-dependencies.svg

clean:
	dune clean --root .
	rm -f grammar.html
	rm -f ecomp ecomp-run ecomp-report
	@if [ -f "$(TESTS_DIR)/Makefile" ]; then \
		$(MAKE) -C "$(TESTS_DIR)" clean OUTPUT_DIR="$(TEST_OUTPUT_DIR)"; \
	else \
		echo "Skipping test cleanup: ecomp-tests was not found at $(abspath $(TESTS_DIR))."; \
		echo "Set TESTS_DIR=/path/to/ecomp-tests to clean its generated results."; \
	fi

test: check-tests
	$(MAKE) -C $(TESTS_DIR) \
		COMPILER=$(abspath ecomp) \
		RUNNER=$(abspath ecomp-run) \
		REPORTER=$(abspath ecomp-report) \
		BUILD_ROOT=$(CURDIR) \
		OUTPUT_DIR="$(TEST_OUTPUT_DIR)" \
		SUITE="$(SUITE)"
