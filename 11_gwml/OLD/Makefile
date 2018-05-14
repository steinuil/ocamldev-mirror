all: byte opt

include Makefile.config

include Makefile.cdk

INSTALLBIN=$(BINDIR)
INSTALLLIB=$(LIBDIR)/gwml-$(GWML_VERSION)-$(GWML_RELEASE)

SRC=-src
INCLUDES=-pp camlp4o `cdk_config  -c $(SRC) extlib wxlib toplevel unix str`
LIBS=-pp camlp4o `cdk_config $(SRC) wxlib toplevel unix str`
OPTLIBS=-pp camlp4o `cdk_config -opt $(SRC) wxlib toplevel unix str`

ESD=esd
IMLIB=imlib

OFLAGS= -w mv

AUTOLOAD=configurator.cmo
AUTOLOAD_ML=configurator.ml
AUTOLOAD_CMI=configurator.cmi

CONFIG= cpp.cmo sendEvent.cmo image.cmo opttable.cmo stdconfig.cmo animate.cmo  stdvirtual.cmo user.cmo stdmenus.cmo stdicon.cmo  stdplacement.cmo themes.cmo stddeco.cmo appMgr.cmo  modules.cmo fvwm.cmo wmaker.cmo afterstep.cmo twm_t.cmo twm_l.cmo twm_p.cmo twm.cmo group.cmo gnome.cmo  undo.cmo dvroom.cmo stdgwmlrc.cmo gwmlrc.cmo 

CONFIG_ML= cpp.mll sendEvent.ml image.ml opttable.ml stdconfig.ml animate.ml  stdvirtual.ml user.ml appMgr.ml  stdmenus.ml stdicon.ml  stdplacement.ml themes.ml stddeco.ml modules.ml fvwm.mll wmaker.mll afterstep.mll twm_t.ml twm_l.mll twm_p.mly twm.ml group.ml gnome.ml  undo.ml dvroom.ml stdgwmlrc.ml gwmlrc.ml 

CONFIG_CMI=$(CONFIG:.cmo=.cmi)

COBJS=$(IMLIB_COBJS) $(ESD_COBJS)
CFLAGS=$(IMLIB_CFLAGS) $(ESD_CFLAGS)
CLINK=$(IMLIB_CLINK) $(ESD_CLINK)

WIDGETS= wob.cmo plug.cmo top.cmo client.cmo screen.cmo null.cmo label.cmo ledit.cmo pixmap.cmo bar.cmo text.cmo
OBJS= gwml_version.cmo gwml_args.cmo locale.cmo wobenv.cmo imager.cmo sound.cmo gwml.cmo gradients.cmo $(WIDGETS)

MAIN=main.cmo

TMPFILES= twm_p.mli twm_p.ml twm_l.ml fvwm.ml wmaker.ml cpp.ml afterstep.ml

gwml_install: Makefile
	rm -f gwml_install
	echo "#!/bin/sh" > gwml_install
	echo CONFIG_DIR='$$HOME'/.gwml-$(GWML_VERSION) >> gwml_install
	echo echo Installing in '$$CONFIG_DIR' >> gwml_install
	echo mkdir -p '$$CONFIG_DIR' >> gwml_install
	echo cd $(INSTALLLIB)/config >> gwml_install
	echo cp -f "*" '$$CONFIG_DIR'/ >> gwml_install
	echo touch '$$CONFIG_DIR'/.depend >> gwml_install
	echo cd '$$CONFIG_DIR' >> gwml_install
	echo make depend >> gwml_install
	echo echo Installation done >> gwml_install
	echo echo Don''t forget to compile the files with make >> gwml_install
	echo echo after modification >> gwml_install
	chmod +x gwml_install
	rm -f config/Makefile
	echo > config/Makefile
	echo CONFIG=$(CONFIG) $(AUTOLOAD) >> config/Makefile
	echo >> config/Makefile
	echo TMPFILES=$(TMPFILES) >> config/Makefile
	echo >> config/Makefile
	echo all: "\$$""(TMPFILES)" "\$$""(CONFIG)" >> config/Makefile
	echo >> config/Makefile
	echo INCLUDES=-I $(INSTALLLIB)   >> config/Makefile
	echo depend: fdepend >> config/Makefile
	echo >> config/Makefile
	echo include Makefile.cdk >> config/Makefile
	echo include .depend >> config/Makefile

######################### special rules

tmpfiles: $(TMPFILES)

byte: $(TMPFILES) gwml.byte $(CONFIG) $(AUTOLOAD) tmpfiles gwml_install
	cp -f gwml.byte gwml

opt: $(TMPFILES) gwml.opt $(CONFIG) $(AUTOLOAD) tmpfiles gwml_install
	cp -f gwml.opt gwml

top: 
	ocamlmktop -custom -o top unix.cma str.cma -cclib "-lunix -lstr"

gwml.byte: $(OBJS) $(COBJS) $(MAIN)
	$(OCAMLC) -custom -o gwml.byte unix.cma $(LIBS) $(OBJS) $(MAIN) $(COBJS) $(CLINK)

gwml.opt: $(OBJS:.cmo=.cmx) $(COBJS) $(MAIN:.cmo=.cmx)
	$(OCAMLOPT) $(INCLUDES) -o gwml.opt $(OPTLIBS) $(OBJS:.cmo=.cmx) $(MAIN:.cmo=.cmx) $(COBJS) $(CLINK)

gwml.static: $(OBJS:.cmo=.cmx) $(OPT_DEP_X) $(OPT_DEP_COM) $(OPT_DEP_DYN) $(OPT_DEP_CCR) $(COBJS) $(CONFIG:.cmo=.cmx) $(MAIN:.cmo=.cmx)
	$(OCAMLOPT) $(INCLUDES) -o gwml.static  $(OPT_ALL_X) $(OPT_LINK_COM) $(OPT_LINK_DYN) $(OBJS:.cmo=.cmx) $(CONFIG:.cmo=.cmx) $(MAIN:.cmo=.cmx) $(COBJS) $(CLINK)

install-static: gwml.static
	cp -f gwml.static $(installroot)$(INSTALLBIN)/gwml.static

install-icons:

THEMESDIR=$(CURDIR)/themes

install-themes:
	mkdir -p $(installroot)/usr/local/share/WindowMaker/
	(cd $(installroot)/usr/local/share/WindowMaker; \
	tar xf $(THEMESDIR)/WindowMaker.tar)
	mkdir -p  $(installroot)/usr/local/share/afterstep/desktop/themes
	(cd $(installroot)/usr/local/share/afterstep/desktop/themes;\
	tar xf $(THEMESDIR)/AfterStep.tar)
	mkdir -p $(installroot)/usr/local/share/GwML
	(cd $(installroot)/usr/local/share/GwML; tar xf $(THEMESDIR)/GwML.tar)

themes-tar:
	rm -rf themes.copy
	cp -dpR themes themes.copy
	(cd themes.copy; find . -name CVS -exec rm -rf {} \; ; echo)
	(cd themes.copy/WindowMaker; tar cf ../../themes/WindowMaker.tar .)
	(cd themes.copy/AfterStep; tar cf ../../themes/AfterStep.tar .)
	(cd themes.copy/GwML; tar cf ../../themes/GwML.tar .)
	rm -rf themes.copy

install-config: $(CONFIG) $(AUTOLOAD) gwml_install
	mkdir -p  $(installroot)$(INSTALLLIB)/config
	cp -f $(CONFIG_ML) $(AUTOLOAD_ML)  $(installroot)$(INSTALLLIB)/config
	cp -f config/Makefile Makefile.cdk config/README $(installroot)$(INSTALLLIB)/config
	cp -f gwml_install $(installroot)$(INSTALLBIN)/
	cp -f FAQ.gwml $(installroot)$(INSTALLLIB)

install: install-config
	mkdir -p $(installroot)$(INSTALLBIN)
	cp -f gwml.byte $(installroot)$(INSTALLBIN)/gwml.byte
	mkdir -p $(installroot)$(INSTALLLIB)
	cp -f *.cmi $(CONFIG) $(AUTOLOAD) $(installroot)$(INSTALLLIB)
	cp -f gwml.opt $(installroot)$(INSTALLBIN)/gwml

clean:
	rm -f *.cm? *.mlg *.o gwml gwml.byte gwml.opt *~ *.output core *.s $(TMPFILES) gwml.gz gwml.static top log 

depend: tmpfiles fdepend

tmp_p.mli: tmp_p.ml


include .depend


