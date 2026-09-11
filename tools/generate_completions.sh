#!/bin/sh
set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$root"

dune build src/main.exe src/runner.exe src/report_main.exe
mkdir -p completions/bash completions/zsh completions/fish

for command in ecomp ecomp-run ecomp-report; do
  case "$command" in
    ecomp) executable=_build/default/src/main.exe ;;
    ecomp-run) executable=_build/default/src/runner.exe ;;
    ecomp-report) executable=_build/default/src/report_main.exe ;;
  esac
  "$executable" --completion bash > "completions/bash/$command"
  "$executable" --completion zsh > "completions/zsh/_$command"
  "$executable" --completion fish > "completions/fish/$command.fish"
done
