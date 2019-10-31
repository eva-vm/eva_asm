# Eva Asm

This repo contains the standard assembler for the Eva assembly language. The documentation can be found [here](mini_doc.md).

# Installation

This assembler is written in ocaml. In order to install it, you may need to install [ocaml/opam](https://ocaml.org/docs/install.html) first.

## Installation process


**[1] clone the repo**

```
  $ git clone https://github.com/eva-vm/eva_asm.git
```

**[2] package installation**

```
  $ cd eva_asm
  $ opam install .
```

**[3] testing ?**

Even if this project is still a WIP, a minimal test version can be used as follow :

```
  $ evasm_test
  ... type your asm code
  ... ctrl-d
  ... outputs the binary in test.evasm
```
