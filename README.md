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

## Lab 2
The goal of this lab is to walk away with a complete program analyzer for a simplified "while" programming language. The program analyzer given in this lab follows a line of recent research into algebraic program analysis. For further information browse the publications of [Zak Kincaid](cs.princeton.edu/~zkincaid/) and [Tom Reps](pages.cs.wisc.edu/~reps/)

Note: If you are viewing this README in a markdown viewer the rest of the README should render correctly. If viewing as raw text there are additional backslash characters to escape markdown characters. You can view the rendered markdown on the lab2 branch of the [github repository](https://github.com/cyphertjohn/compositionalanalyzer).

Here is the basic grammar for a "while" program P, where x ranges over variables and n ranges over natural numbers. 

 - P -\> assume (Bool) S assert (Bool) \| assume(Bool) S \| S assert (Bool) \| S
 - S -\> Loop \| Branch \| Assignment \| S S
 - Loop -\> while \(Condition\) do S done
 - Branch -\> if \(Condition\) then S endif \| if \(Condition\) then S else S endif
 - Assignment -\> x = Linexp;
 - Linexp -\> Linterm \+ Linexp \| Linterm \- Linexp \| \- Linterm \| Linterm
 - Linterm -\> n \| x \| nx \| n\*x
 - Condition -\> nondet \| Bool
 - Bool -\> true \| false \| ( Bool ) \| Bool && Bool \| Bool \|\| Bool \| \!Bool \| Pred
 - Pred -\> Arith <= Arith \| Arith \>= Arith \| Arith == Arith \| Arith < Arith \| Arith \> Arith
 - Arith -\> Linexp \| Linexp mod Linexp

To see some example programs look in the `examples` directory. The semantics of these programs is that all variables have integer type and are assumed to have been already initialized to some unknown value. Programs can also start with an `assume` which constrains the incoming values of program variables. Programs can also end with an `assert`. In this case the analyzer will attempt to prove whether the expression in the assertion holds for all possible initial states.

The idea of this analyzer is to summarize the transition relation of a while program. That is, consider some program P. Let **y** denote the values of variables at the start P, **y'** denote the values of variables after P, and let **y**P**x'** denote that **y'** is the post state if P runs given the pre-state assignment **y**. The transition relation of P is then the union of all possible (**y**, **y'**) such that **y**P**y'** holds. The analyzer will summarize this relation with a *transition formula* F(**x**, **x'**) over pre-state variables **x** and post-state variables **x'**. A transition formula, F(**x**, **x'**), is sound if
> For every (**y**, **y'**) if **y**P**y'** holds then F(**y**, **y'**) also holds.

A sound transition formula can then be used to check assertions or some other down-stream program analysis task.

### Exercise 1
First we will get a feel for transition formulas. Go ahead and make `analyzer.native` with `make` if you haven't already. `analyzer.native` takes in a while file as input and prints out a transition formula that summarizes the input file. If the input program ends in an assertion the analyzer will attempt to prove the assertion using the generated transition formula. If `analyzer.native` prints "Assertion PASSED", then the assertion always holds. If `analyzer.native` prints "Assertion FAILED", then the analyzer is unable to prove the assertion, and thus the assertion may or may not hold.

Take a look at `examples/ex1.while`. This simple program takes in two positive integer variables `x` and `y` and updates their values. Run
```
./analyzer.native -v debug examples/ex1.while
```
to see the summary for `examples/ex1.while`.

Now examine `examples/if_ex1.while`. This program contains a branch while depends on the value of some initialized variable `x`. If `x` is positive `y` increase by one, otherwise `y` decreases by one. In either case the value of `x` remains unchanged. It is also clear that the assertion at the end of the program does *not* hold. Run
```
./analyzer.native -v debug examples/if_ex1.while
```
to see the summary for `if_ex1.while`. The analyzer correctly responds that the assertion failed, and also produces the procedure summary for `if_ex.while`.

Program summaries produced by `analyzer.native` consist of two parts: a *transform* and a *guard*. The guard is a logical formula over pre-state program variables and other *skolem variables*. A skolem variable in the context of this analyzer is a variable that is implicitly existentially quantified. The transform of summary is a list of equations where the lhs of each equation is a post-state variable and the right hand side is a term consisting over pre-state variables and skolem variables. The way to understand a summary is to say that the value of primed variables equals the rhs's of the transform when the guard formula holds. Note that if a program variable is absent from the transform, then it is implicit that that variable is un-modified by the program. The variable `x` in `if_ex1.while` gives such an example.

> Make a small modification to the expression in the assume of `if_ex1.while` so the assertion passes.

### Exercise 2
In this exercise we will explore how the analyzer builds summaries of loop-free code.

First, we must understand the intermediate representation the analyzer uses for programs. We represent programs via *path-expressions*. A path-expression for a program P is a regular expression, r, over an alphabet of program statements, such that the language of r equals the set of paths of P.

If you run the analyzer with default verbosity it will omit the procedure summary but still print out the path-expression of the program. Run
```
./analyzer.native examples/ex1.while
```
You should see a path-expression of "cba" with definitions for "a", "b", and "c". This path-expression denotes that there is only one path in the program `examples/ex1.while` consisting of statements "b" followed by "a" with the pre-condition (the assume) "c". If you run
```
./analyzer.native examples/if_ex1.while
```
you should see a path-expression with a \+ indicating a branch in the program. You can also try running the analyzer on a program with a loop to see the path-expression.

For the simple while programming language, extracting a path-expression from a program is simple. Branches correspond to \+, loops to \*, and sequencing to concatenation. In fact this analyzer parses input programs directly as path-expressions in `par.mly`. For languages with more complicated constructs, like `goto`, a more complicated algorithm, such as [Tarjan's algorithm](https://dl.acm.org/doi/10.1145/322261.322273), is required. 

Once a path-expression has been constructed, we must "re-intepret" the regular expression in our analysis domain. For the case of this analyzer that means we must interpret the letters of the path-expression as transition formulas, and interpret the regular expression operators as operations on transition formulas.

It should be clear how to transform a single program statement or condition to a transition formula.

- For an assignment `x = rhs`, we get the formula `x' = rhs`.
- For a condition we simply interpret the condition as a formula.

For the regular expression constants 0 and 1 we have:

- 0 := false
- 1 := /\\ x' = x for each program variable x.

What remains is how to intepret the dot and \+ of regular expressions as operations on transition formulas.

- \+ corresponds to a branch or choice, so 

 > f1(**x**, **x'**) \+ f2(**x**, **x'**) := f1(**x**, **x'**) \\/ f2(**x**, **x'**)

- dot, or extend, corresponds to sequencing, so essentially we substitute the pre-state of f2 with the post-state of f1. Logically,

 > f1(**x**, **x'**) f2(**x**, **x'**) := exists **x''** f1(**x**, **x''**) /\\ f2(**x''**, **x'**)

With these interpretations we can construct a program summary for any loop free code. Re-run
```
./analyzer.native -v debug examples/ex1.while
```
The resulting transition formula was constructed by interpeting "c", "b", and "a" as transition formulas and using the extend operation to sequence them together.

Similarly if you re-run
```
./analyzer.native -v debug examples/if_ex1.while
```
The analyzer constructed this summary by interpreting dc and ba as transition formulas, or\-ing those together, and then pre-pending the interpretation of fe to the result.

The implementation for intepreting path-expressions of loop-free code can be found in `transition.ml`. The intepretations for program statements can be found at around line 240. The interpretation for extend is called mul, and choice is called add at lines 224 and 194 respectively. The add and mul functions are more complicated that what is given above due to the particular representation of a transition formula, but the effectively implemented the definitions above. Finally, whole path-expressions are intepreted with the recursive `eval` function in `cra.ml`.

> Open `max.while` and write a program that sets a variable "max" to the maximum of variables "x", "y", and "z" such that the assertion passes.

Feel free to write a couple of your own on loop-free while programs to get a feel for path-expression and transition formulas.

### Exercise 3
What remains is a way to summarize loops. What one needs to do is given a transition formula f(**x**, **x'**) compute a transition formula f(**x**, **x'**)\* that approximates the transitive closure of f(**x**, **x'**).

The method that is currently implemented in the analyzer, which you can find in `cra.ml` around line 94, uses the following definition for \*.

>f(**x**, **x'**)\* := **x'** = **x** \\/ (exists **y'** f(**x**, **y'**)) /\\ (exists **y** f(**y**, **x'**))

The idea of this method is that we abstract a loop with it's pre-state, exists **y'** f(**x**, **y'**), and post-state, exists **y** f(**y**, **x'**).

Take a look at `examples/loop1.while`. This program has a single program variable x which starts out at 0 and increments to 100. Go ahead and run
```
./analyzer.native -v trace examples/loop1.while
```
The analyzer should print the loop pre-condition and the loop post-condition as well as the overall summary. This summary is enough to prove the assertion.

To see why, note the loop post-state. This formula says that x' <= 100 at the end of every iteration of the loop. Using the above definition for the star the analyzer summarizes (just the loop) as

>x' = x \\/ x < 100 /\ x' <= 100

Then the analyzer tacks on the condition that x >= 100 for the loop to terminate. This, along with the fact that x=0 initially implies that

> x' <= 100 /\\ x' >= 100

which is enough to prove that x' == 100.

Now take a look at `examples/loop2.while`. This program is the same as `examples/loop1.while`, but instead increments x by 2 instead of one. The assertion in this program is true; however if you run
```
./analyzer.native -v trace examples/loop2.while
```
you will see the analyzer is unable to prove the assertion.

>Look at the loop pre-state and post-state and determine by hand what the loop-summary is using the definition of star above. Explain why the analyzer is unable to prove the assertion and then make a small modification to the assertion expression so the analyzer is able to prove the assertion.

### Exercise 4
`loop3.while` gives another example of a true assertion that the analyzer is unable to prove. The problem is that with the above method for computing \* we are introducing too much abstraction. In this exercise we will investigate a more precise method of using *recurrence relations*.

The idea is that to compute f(**x**, **x'**)\*, we will extract a set of implied recurrence relations from f, solve those recurrence relations, and convert the solution back as a transition formula.

Recall `examples/loop1.while`. The loop-body formula is

> x < 100 /\\ x' = x + 1

Let k be some natural number which we will use to denote the values of variables on the k'th iteration of the loop. That is, let x\_[k] denote the value of variable x on the k'th iteration of the loop. By inspection of the loop body formula we can see

>x\_[k+1] = x_[k] + 1

which has the solution

>x\_[k] = x_[0] + k

This solution can be converted back to a formula yield the following summary

> x' = x \\/ exists k >= 1. x' = x + k

We can also use the strategy from exercise 3 and add the pre-state and post-state to the case where the loop iterates for more precision

> x' = x \\/ exists k >= 1. x' = x + k /\ x < 100 /\ x' <= 100

In this exercise we will slightly modify the solver to implement this strategy.

First, we must target a class of recurrences that we will extract and solve. For this lab we will target *linear recurrence equations*, (for a more complicated class of recurrences see [Non-linear Reasoning for Invariant Synthesis](https://dl.acm.org/doi/10.1145/3158142)). A linear recurrence equation has the form

> **ax**_[k+1] = **ax**\_[k] + b

where **x** is a vector of size n of program variables **a** is a vector of size n of constants and b is a constant. We can write a set of linear recurrence equations using a matrix A and vector **b**.

> A**x**_[k+1] = A**x**\_[k] + **b**

The solution to such an equation is

> A**x**_[k] = A**x**\_[0] + k\***b**

for distinguished loop counter k.

To see the utility of linear recurrences, navigate to `cra.ml`, and uncomment the code block from approximately lines 97-103 in the `star` function. What the analyzer will now do is print out the set of recurrence relations it extracts when run with debug verbosity. Run `make`.

Now take a look at `examples/loop_branch.while`. The loop in this program has a branch, and thus one might assume that there is no recurrence relation capturing the dynamics of x or y that is implied by the loop body. While it is the case that there is no recurrence for x or y, there is a recurrence for the *term* x+y. That is we have

> (x + y)_[k+1] = (x+y)\_[k] + 1

If you run `./analyzer.native -v debug examples/loop_branch.while` you should see this recurrence relation as output.

### Exercise 5
In this exercise we investigate how the analyzer extracts linear recurrence equations. This process is implemented in `cra.ml` at about line 40 if you would like to follow along.

1. Abstract the loop body formula to a set of affine equations using `alpha_from_below` from the first lab. This procedure extracts the *best* set of affine equations implied by the loop body. That is a matrix-vector equation E[**xx'**] = e, where [**xx'**] is a column vector of primed and unprimed program variables.

2. For each program variable x, introduce a delta variable, d, and add the equation d = x' - x to the set of affine equatoins. Each delta variable represents the change to variable x in the loop body. Our set of affine equations, now looks like
> E'[**xx'd**] = e' for some E' and e'.
3. We then project out all the variables except the **d** variables from the set of affine equalities. We are left with the matrix equation
>A**d** = **b** for some A and **b**.
4. Recall that the **d** variables are defined to be equal to **x'**-**x**. Thus from the previous equation we can directly extract
>A**x'** = A**x** + **b**

Furthermore, this process extracts the *best* set of linear recurrences in the sense that any other implied linear recurrence will be implied by the set of recurrences extracted. This demonstrates the power of `alpha_from_below`.

Now, the only thing that remains to be implemented is to solve the set of extracted recurrence relations. You're goal is to implement the function `solve_rec` on line 83 of `cra.ml`. The purpose of `solve_rec` is to take in a `lin_rec` and produce a `lin_rec_sol`. That is take in a single linear recurrence, something of the form **ax**\_[k+1] = **ax**\_[k] + b, and produce the recurrence solution **ax**\_[k] = **ax**\_[0] + k*b. Also make sure to comment and uncomment the marked lines in `star` so `solve_rec` gets called. The implementation of `solve_rec` should be 1 line. The type definitions of `lin_rec` and `lin_rec_sol` can be found in `sigs.mli`.

Once `solve_rec` is implemented, `analyzer.native` will be a complete analyzer for while programs. `analyzer.native` will be able to prove all the assertions for the programs with loops used in these exercises, without modification.

An interesting example to try is `examples/division_by_3.while`, which is a contrived implementation of a program that computes the remainder and divisor of x by 3. The assertion at the end of the program requires that the program indeed does compute the right values. Furthermore, the recurrence version of the analyzer will be able to prove that this assertion always holds.

This concludes lab 2. Feel free to create your own while programs, and run `analyzer.native` to see what happens.
