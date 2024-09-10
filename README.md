# fun-ocaml-workshop

Fun OCaml workshop

## Install

```shell
$ opam pin add . --with-version=dev
```

## Test

```shell
# Run the server:
$ dune exec funocaml_server

# Check http://localhost:8080/

# Run some actors:
$ dune exec -- ./test/test.exe your-name
$ dune exec -- ./test/test.exe other-name
```
