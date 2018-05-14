# ocamldev
A collection of old OCaml libraries and programs by Fabrice Le Fessant, formerly
hosted on [his website](http://fabrice.lefessant.net/), recovered from the
[Wayback Machine](https://web.archive.org/web/20061123050647/http://pauillac.inria.fr/~lefessan/src/).

I salvaged these libraries for research purposes; I don't intend to continue
development or maintain them. I doubt most of this code even compiles, the last
version some of these seem to have been tested with was 3.08, and some of the
libraries rely on deprecated features like mutable strings. Efuns and GwML were
last updated in 1999, so they most likely need heavy rework to work under a
recent version of OCaml.

If you're crazy enough to want to continue development on any of these, I
suggest you clone the repo and reupload it to your account instead of forking
it, and maybe open an issue or a PR so that I can put a link to your repo on
this readme.

The license was only included in `ocamlconf` and in short form on top of the
files in `extlib`, so I assumed GPLv2 for the rest of the libraries and programs
as well.


#### ocamlconf
A sort of build system that generates a `./configure` script and a `Makefile`
for OCaml projects based on an `ocamlconf.ocf` file. Used to build some of the
other libraries.

#### extlib
A bunch of generally useful modules, more or less documented.

#### unix2
Utility modules for Unix things.

#### xlib
An implementation of Xlib in OCaml.

#### wxlib
A "really BETA" widget toolkit for Xlib.

#### efuns
A text editor similar to Emacs.

#### gwml
A window manager configurable in OCaml.


# Original README.txt
In this directory, you will find some of my Objective-Caml work:

01\_ocamlconf: a program you need to compile everything else...

    cd 01_ocamlconf; ./configure; make; sudo make links

For other stuff, just untar all of them somewhere, and call
"ocamlconf; ./configure; ocamlmake" (ocamlconf and ocamlmake are
provided by the previous package, and generate the needed "configure"
and "Makefile")

02\_extlib, 03\_unix: some useful libraries
04\_xlib, 05\_wxlib: Xwindow library (works great) and widgets (works bad)

10\_efuns: an Emacs clone
11\_gwml: a Window-Manager

Most directories contain a "ocamlconf.ocf" file, used by
ocamlconf/ocamlmake to generate the Makefiles... Modify them if you
have problems compiling this stuff.

Fabrice LE FESSANT
INRIA-Futurs
