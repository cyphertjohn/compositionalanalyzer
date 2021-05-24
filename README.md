# SSFT21

## Dependencies
The analyzer is written in ocaml and requires ocamlbuild, Z3, and an ocaml interface to the GNU MultiPrecision library (gmp). These packages can be installed with opam. To install opam and gmp run:
```
sudo apt-get install opam libgmp-dev libmpfr-dev
```
Then if opam hasn't been initialized run:
```
opam init
```
Then the ocaml packages can be installed with
```
opam install ocamlbuild mlgmpidl z3
```
Installing z3 may take a while.

## Lab 1