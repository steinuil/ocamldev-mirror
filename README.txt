In this directory, you will find some of my Objective-Caml work:

01_ocamlconf: a program you need to compile everything else...
  cd 01_ocamlconf; ./configure; make; sudo make links

For other stuff, just untar all of them somewhere, and call
"ocamlconf; ./configure; ocamlmake" (ocamlconf and ocamlmake are
provided by the previous package, and generate the needed "configure"
and "Makefile")

02_extlib, 03_unix: some useful libraries
04_xlib, 05_wxlib: Xwindow library (works great) and widgets (works bad)

10_efuns: an Emacs clone
11_gwml: a Window-Manager

Most directories contain a "ocamlconf.ocf" file, used by
ocamlconf/ocamlmake to generate the Makefiles... Modify them if you
have problems compiling this stuff.

Fabrice LE FESSANT
INRIA-Futurs