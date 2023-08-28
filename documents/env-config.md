# EIO

These instructions do not work
https://github.com/ocaml-multicore/eio/blob/main/HACKING.md

```
opam switch create 5.0.0~beta1
opam install utop
```

my packages were too old, so also

```
opam update && opam upgrade
```

Somehow it pins version `0.6` and sometimes `dev`. Instead, I used the included
dockerfile which shows how to pin to a specific version.

```
opam pin --with-version=dev . -yn
opam install eio_main
```

Now you can run the examples from the Readme.

## OCaml Platform

To use the ocaml platform extension for vscode we want to be able to compile the
project with dune. The `@all` target requires some extra dependencies.

```
opam install crowbar dscheck mdx alcotest fmt
dune build
```

# Hazel

Master branch failed compiling. Trying out the pop21artifact branch
from now on.

```
git clone https://gitlab.inria.fr/cambium/hazel.git
./setup.sh
# afterwards
make all -j 8
```

## Coq Unicode

Follow these instructions.
https://gitlab.mpi-sws.org/iris/iris/-/blob/master/docs/editor.md
