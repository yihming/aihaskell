aihaskell
=========

Abstract Interpretation Framework written in Haskell.

## Goal

Perform the static numerical analysis on C-like programs using Abstract Interpretation.

## INSTALL

* Before building the project, you need to install [Apron](http://apron.cri.ensmp.fr/library/) (an open-source software built by INRIA, France.) and [PPL](http://bugseng.com/products/ppl/) (Parma Polyhedra Library, developed at University of Parma, Italy).
* After that, type _make all_.

## Implementation

* Prototype language:
  * Defined in _AbstractSyntax.hs_. C-like syntax.
* Front-end (_FrontEnd.hs_):
  * Using Haskell with compiler writing package _Parsec_.
  * Intermediate Representation is an AST encapsuled as a Haskell class.
* Back-end (_SemanticsAnalysis.hs_):
  * Automatically construct the interface of Apron and PPL using Haskell FFL.
  * Process the numerical values of program variables in AST, then assign polyhedron domains for them along with the program data flow.
  * Print out the source code with numerical values of program variables at each line inside the comment.
  * Abstract values of program variables are represented as polyhedra consisting of inequality constraints.

The whole analysis procedure is static, which means you don't need to really execute the program to check the numerical safety in the runtime. Instead, this work is done at the compile time.

## Reference

INRIA already provides an online analyzer to see the power of abstract interpretation: [Interproc](http://pop-art.inrialpes.fr/interproc/interprocweb.cgi). You can try it with the default convext polyhedra domain, or other interesting domains like intervals, octagons, etc.
