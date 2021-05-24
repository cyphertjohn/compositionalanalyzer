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

## Lab 1
The goal of this lab is to illustrate how logic makes a good intermediate representation for abstract interpretation, and how we can use advances in SMT, present in this project using Z3, to do a lot of the heavy lifting of implementing program analysis. The exercises will have you examine the ["alpha from below"](http://www.cs.cornell.edu/courses/cs711/2005fa/papers/rsy-vmcai04.pdf) (figure 1) algorithm which extracts the best abstract value from a logical formula. You will then examine how using logic as an intermediate representation allows one to implement a "reduced product" quite easily.

When this lab is compiled with `make`, an executable `analyzer.native` will be produced. `analyzer.native` takes one or two "form" files as input. A "form" file is a custom file that contains a linear integer arithmetic (LIA) formula. See the examples directory for examples of the syntax of these formulas. `par.mly` gives the detailed grammar.

Also in this project are two abstract domains. A parity domain and a domain of affine equalities. The affine equality domain is complete, but the parity domain has some functionality missing, which you will provide in the first exercise. When provided with a single "form" file `analyzer.native` will parse the file and convert it to a Z3 expr, essentially a LIA formula. `analyzer.native` will attempted to abstract this formula in the parity domain and print the result; however, this functionality is not implemented yet. If `analyzer.native` is provided with two "form" files, it will attempt to abstract the first to the parity domain and the second to the domain of affine equalities and print both results.

### Exercise 1
As a warm-up for the first exercise we will examine the parity domain and implement the join. Navigate to `parity.ml`, and look at the functions there. Read the comments to get a sense of how a domain is implemented. The join function at the bottom of the file has a TODO comment, asking you to write a few lines of code.

One thing to consider is how few functions are required to implement the domain. We really only need a join and meet and a way to convert to and from logic. If we didn't take this logical approach we would have to implement abstract transfer functions for all the expressions in our language! That is, we would have to determine how our abstract values change with all the various arithmetic operations (i.e. Even + Even = Even, Even + Odd = Odd, etc.)

### Exercise 2
Once the parity join is complete, `parity.ml` gives functions to manipulate domain elements, and a way to convert a parity element to logic. What remains is a way to go from logic to the parity domain. This is our next goal. Navigate to abstract.ml. There you will find an incomplete implementation of alpha_from_below. The next exercise is to fill in that function.

Once completed, you will have a way to extract a parity element from a formula. Go ahead and try a few examples. Try
```
./analyzer.native examples/parity1.form
```
You should get something like z->Odd, y->Even, and x->Even. Try some of the other examples and try to make your own.

### Exercise 3
The next goal is to combine the parity domain with another domain to explore product domains. The domain we will combine with is the domain of affine equalities. An element of the domain of affine equalities is a constraint system of the form *Ax = b*, where *A* is a m by n matrix, *x* is a vector of n variables, and *b* is a vector of m constrants.

Go ahead an browse `affine.ml`. You don't have to understand all the details, but observe that `affine.ml` implements the same abstract domain functions as `parity.ml`, (along with some other helpers that will be used in lab 2). The domain functions in `affine.ml` may seem complicated, but they mostly boil down to computing reduced row echelon forms of matrices. 

Because `affine.ml` implements the same abstract domain functions as `parity.ml`, we can use the same alpha_from_below function you wrote for the previous exercise to extract affine equalities from formulas.
If you provide two input files to `analyzer.native`, the first file will be abstracted to the parity domain and the second file will be abstracted to the affine equality domain. Try a few examples,
```
./analyzer.native examples/parity1.form examples/eq1.form
```
Try with verbosity set to trace `-v trace` to see how alpha from below is working.

At first glance, one might think that the domain of affine equalities doesn't provide an interesting use case for `analyzer.native`. The only type of expressions allowed in a "form" file are boolean combinations of linear expressions. If we have a conjunction of linear equations as an input formula one can nearly read off the set of affine equalities that hold, and if we have a disjunction of linear equations we must abstract away what happens on either side of the disjunction (unless the disjunction is trivial e.g. x == 1 || x == 1.). Try
```
./analyzer.native examples/parity1.form examples/eq3.form
```
We don't retain any information about y or z. However, the true reality is a little more subtle. Can you write a formula with a non-trivial disjunction such that `affine.ml` retains some information about the variables on either side of the disjunction? If you can't come up with an example, that's ok. It's not critical for the rest of this lab, but we will use affine equalities more in the second lab when we build a full program analyzer.

### Exercise 4
Now that we have two implemented domains, we can explore the ideas of reduced product. The goal is given two domains, create a third that combines the power of each. We can do this fairly simply by creating a pair of domain elements. That is, suppose we want to combine domain A with domain B. Suppose the type of the elements of A is A.t and the type of elements of B is B.t. We can create a product domain whose elements are of type (A.t, B.t). Then for each domain function we just use the functions of A and B pairwise on an element of type (A.t, B.t). This construction can be found at the bottom of `abstract.ml`. This construction works and gives a sound implementation. 

To see how this domain works you can add `-prod` to `analyzer.native` when giving `analyzer.native` two formula files. `analyzer.native` will abstract the first formula file using the parity domain, abstract the second file using the affine equality domain, and compute the product of these two elements and print the result. Try
```
./analyzer.native examples/parity3.form examples/eq2.form -prod
```
You should see that the product domain value is simply the tuple of the parity domain and the affine equality domain.

However, because we are using logic as an intermediate representation, we can do better. What the logic space does is allow the element of one domain to influence an element of the other domain. This idea is known as "reduced product".

A skeleton for an implementation of reduced product can be found at the bottom of `abstract.ml`. Right now that function just returns a pair. Can you implement the reduced product and improve precision?

If you implemented reduced product correctly, if you now try
```
./analyzer.native examples/parity3.form examples/eq2.form -prod
```
you should see that the affine equality domain has influenced the parity domain.

### Exercise 5
The previous exercise shows an example where the affine equality domain influenced the parity domain. Can you come up with an example where both domain elements influence each other? That is, come up with a parity formula and a affine equality formula such that reduced_product a b = (x, y) has x less than a and y less than b in their respective lattices.
