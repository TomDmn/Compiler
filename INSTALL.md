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
    - batteries
    - yojson
    - websocket
    - websocket-lwt-unix
4. Install Alpaga from its sibling checkout
    > opam pin add alpaga ../alpaga
5. Build the project
    > make
