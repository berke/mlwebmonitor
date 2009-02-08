# make all: 		make bytecode archive
# make opt: 		make native archive
# make install: 	install bytecode archive, and if present, native archive
# make uninstall: 	uninstall package
# make clean: 		remove intermediate files
# make distclean: 	remove any superflous files
# make release: 	cleanup, create archive, tag CVS module 
#			(for developers)

#----------------------------------------------------------------------
# specific rules for this package:

OBJECTS  = \
	   util.cmo \
	   bool.cmo \
	   www.cmo \
	   opt.cmo \
	   target.cmo \
	   action.cmo \
	   parser.cmo \
	   monitor.cmo
XOBJECTS = $(OBJECTS:.cmo=.cmx)
EXEC     = monitor
XEXEC	 = monitor.opt
NAME     = monitor
REQUIRES = netstring netclient

BYTE_THREAD = -thread
NAT_THREAD = -thread

.PHONY: all
.PHONY: opt
.PHONY: bt

all: opt bt
opt: $(XEXEC)
bt: $(EXEC)

$(EXEC): $(OBJECTS) 
	$(OCAMLC) -o $(EXEC) -linkpkg $(OBJECTS)

$(XEXEC): $(XOBJECTS)
	$(OCAMLOPT) -o $(XEXEC) -linkpkg $(XOBJECTS)

#----------------------------------------------------------------------
# general rules:

OPTIONS   = 
OCAMLC    = ocamlfind ocamlc -pp camlp4o.opt -dtypes -g $(OPTIONS) -package "$(REQUIRES)"
OCAMLOPT  = ocamlfind ocamlopt -pp camlp4o.opt -dtypes $(OPTIONS)   -package "$(REQUIRES)"
OCAMLDEP  = ocamldep -pp camlp4o $(OPTIONS)
OCAMLFIND = ocamlfind
OCAMLDOC  = ocamlfind doc -package "$(REQUIRES)"

depend: *.ml *.mli
	$(OCAMLDEP) *.ml *.mli >depend

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa *.annot
	rm -f $(EXEC) $(XEXEC)
	[ ! -d tests ] || $(MAKE) -C tests clean
	rm -rf doc

.ml.cmx:
	$(OCAMLOPT) -c $<

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.SUFFIXES: .cmo .cmi .cmx .ml .mli

.PHONY: install install_bt install_natives install_bytes install_indep

DESTDIR =

install_natives: all
	mkdir -p $(DESTDIR)/usr/bin
	install -m 755 monitor.opt     $(DESTDIR)/usr/bin/monitor
	mkdir -p $(DESTDIR)/usr/share/doc/monitor $(DESTDIR)/usr/share/man/man8
	install -m 644 grammar.bnf     $(DESTDIR)/usr/share/doc/monitor/grammar.bnf
	install -m 644 edos.config     $(DESTDIR)/usr/share/doc/monitor/edos.config
	install -m 644 README          $(DESTDIR)/usr/share/doc/monitor/README
	install -m 644 monitor.8       $(DESTDIR)/usr/share/man/man8/monitor.8
	gzip -9                        $(DESTDIR)/usr/share/man/man8/monitor.8

install_indep:

install: install_natives install_indep
install_bt: install_bytes install_indep

include depend
