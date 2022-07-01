# OSTRICH-OCaml


## General info

This is a reference implementation of the OSTRICH language, a type-safe rich templating language for OutSystems.  
Note: This project was made public for reviewing purposes for MODELS 2022.


## Content

#### Source code

The project source code is in the folder *./ostrich_proj/lib/*
* *syntax.ml*: defines the syntax of terms and types of the language
* *typing.ml*: defines the typing algorithm
* *eval.ml*: defines the result of terms' evaluation
* *utils.ml*: defines some auxiliary functions
* *prettyPrint.ml*: defines a pretty printer for terms and types


#### Evaluation

We structurally define the most commonly used OutSystems templates in *./ostrich_proj/test/benchmark.ml*, using the syntax defined in the source code.  
In this file, we define all the templates mentioned in the paper. This includes 10 *ready-to-use* templates, and also extra auxiliary templates that define common patterns to some of those 10 templates.  
For each template definition:  
1) we test if it is correctly typed;  
2) we print the result of evaluating its instantiation; and  
3) we print the result of its evaluation at the corresponding runtime.



## Technologies

This project was created with:
* The OCaml toplevel: version 4.12.0
* Dune, a build system: version 2.8.5



## Execution

To run this project, install the technologies mentioned above. 
We defined all the necessary library dependencies using dune.
Hence, to run the code in *./ostrich_proj/test/benchmark.ml*, just open a command line in your local directory of this project, and type:

```
$ dune build
$ dune test
```