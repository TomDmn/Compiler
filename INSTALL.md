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
