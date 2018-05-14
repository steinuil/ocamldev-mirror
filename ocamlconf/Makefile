include Makefile.config

opt: ocamlconf ocamlmake pa_ocamlconf.cmo pa_ocamlconf.cmx

links: 
	ln -sf $(SRCDIR)/ocamlconf $(OCAMLBIN)/ocamlconf
	ln -sf $(SRCDIR)/ocamlmake $(OCAMLBIN)/ocamlmake
	ln -sf $(SRCDIR)/pa_ocamlconf.cmo $(OCAMLLIB)/camlp4/pa_ocamlconf.cmo
	ln -sf $(SRCDIR)/pa_ocamlconf.cmx $(OCAMLLIB)/camlp4/pa_ocamlconf.cmx
	ln -sf $(SRCDIR)/pa_ocamlconf.cmi $(OCAMLLIB)/camlp4/pa_ocamlconf.cmi
	ln -sf $(SRCDIR)/pa_ocamlconf.o $(OCAMLLIB)/camlp4/pa_ocamlconf.o

install: opt
	cp ocamlconf $(OCAMLBIN)/ocamlconf
	cp ocamlmake $(OCAMLBIN)/ocamlmake
	cp pa_ocamlconf.cm? pa_ocamlconf.o $(OCAMLLIB)/camlpt/

clean:
	rm -f ocamlconf *.o *.cm? ocamlmake

distclean: clean
	rm -f version pa_ocamlconf.ml ocamlconf_config.ml Makefile.config

ocamlconf:   ocamlconf_config.ml ocamlconf_sort.ml ocamlconf.ml
	ocamlopt.opt -pp camlp4o -o ocamlconf unix.cmxa ocamlconf_config.ml ocamlconf_sort.ml ocamlconf.ml

ocamlmake:   ocamlconf_config.ml ocamlconf_sort.ml ocamlmake.ml
	ocamlopt.opt -pp camlp4o -o ocamlmake unix.cmxa ocamlconf_config.ml ocamlconf_sort.ml ocamlmake.ml


pa_ocamlconf.cmo: pa_ocamlconf.ml
	ocamlc.opt -I +camlp4 -pp "camlp4o pa_op.cmo q_MLast.cmo pa_extend.cmo pr_dump.cmo" -c pa_ocamlconf.ml

pa_ocamlconf.cmx: pa_ocamlconf.ml
	ocamlopt.opt -I +camlp4 -pp "camlp4o pa_op.cmo q_MLast.cmo pa_extend.cmo pr_dump.cmo" -c pa_ocamlconf.ml
