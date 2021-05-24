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

## Overview
This repository contains ocaml code that is meant to illustrate some of the ideas presented by Tom Reps at the 2021 Summer School on Formal Techniques. No novelty is claimed with this repository. In fact, the second lab is essentially a simpler and weaker version of [duet](https://github.com/zkincaid/duet).

This repository has integration with merlin, which provides typing and module information to IDE's. For the SSFT participants the school VM has VSCode with ocaml syntax highlighting and merlin integration. However, merlin should automatically be integrated with VIM and Emacs if that is preferred.

For the lab participants the exercises will help guide you through the code. However, documentation can also be built with
```
make doc
```
This will create a directory `analyzer.docdir`. If one opens `analyzer.docdir/index.html` with a browser, one can browse the documentation that way. A good module to view the documentation of would be Log. Most files in the repository have a Logger module which provides various printing functionalities.

This repository contains three branches, main, lab1, and lab2. For the lab branches, some of functionality has been removed and the participants are meant to fill in the required through the lab excercises. Lab 2 will build upon lab 1, but the lab 2 branch will contain all the missing pieces from lab 1. Furthermore, all the functionality and thus the solutions are present on the main branch of this repository.

Before starting a lab be sure to
```
git pull
```
before checking out a lab.

It also wouldn't hurt to run
```
eval $(opam env)
```
to sync opam with your current shell.