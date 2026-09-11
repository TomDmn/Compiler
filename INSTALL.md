# Install with opam
1. Install opam (Ocaml PAckage Manager)
    - https://opam.ocaml.org/doc/Install.html
2. Get a sufficient version of OCaml
    > opam switch create 4.13.0 (or higher)
3. Install packages with OPAM
    > opam install package-name
    - stdlib-shims
    - dune
    - menhir
    - lwt
    - logs
    - yojson
    - websocket
    - websocket-lwt-unix
4. Install Alpaga from its Git repository
    > opam pin add alpaga git+https://gitlab-research.centralesupelec.fr/cidre-public/compilation/alpaga.git
5. Build the project
    > make

# Shell completions

The package installs command-line completions for Bash, Zsh, and Fish.
They are discovered automatically by standard package-manager installations.
For a build used directly from the source tree, run make completions, then
load the appropriate files from completions/bash, completions/zsh, or
completions/fish.
