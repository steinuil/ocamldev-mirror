# ocamldev
A collection of old OCaml libraries and programs by Fabrice Le Fessant, formerly
hosted on [his website](http://fabrice.lefessant.net/), recovered from the
[Wayback Machine](https://web.archive.org/web/20061123050647/http://pauillac.inria.fr/~lefessan/src/).

I salvaged these for research purposes; I cleaned up the directories, made some
small modifications, and added `dune` files to get some of them to compile,
but I don't intend to continue development or maintain them in any way.

If you're crazy enough to continue development on any of these, I suggest you
download the repo, pick what you want to use and reupload it to your account
instead of forking it, and maybe open an issue or a PR so that I can put a link
to your repo on this readme.

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
```
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
```
