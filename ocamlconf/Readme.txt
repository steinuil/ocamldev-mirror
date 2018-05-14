  Ocamlconf is a package containing two programs, "ocamlconf" and
"ocamlmake" which can be used to generate a Makefile for an Ocaml
project. They scan recursively the directory where they are run,
looking for files named "ocamlconf.ocf", which are used to
specify building rules for your project. Examples of such files
can be found in the Palabre project.

"ocamlconf" is used to generate a ./configure script, while
"ocamlmake" is used to generate the Makefile. Thus, their normal
usage is:

# ocamlconf
# ./configure
# ocamlmake
# make

Installation
============

  You need Ocaml at least version 3.08 (http://caml.inria.fr/)

# ./configure
# make
# make install

Author
======
  Fabrice Le Fessant (Fabrice.Le_Fessant@inria.fr)
  distributed under GPL

