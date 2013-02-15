#LEMDIR=$(dir $(shell which lem))

SVR=webserverDNShere

PWD=$(shell pwd)

LEMDIR=../../lem/

ATOMICDIR=../axiomatic/ntc

JSOFOCAMLDIR=js_of_ocaml-1.2
JSOFOCAML=$(JSOFOCAMLDIR)/local-install/bin/js_of_ocaml

PROGS=main 

FILENAME=out

DVIPSFLAGSA4 = -Ppdf -j0 -G0 -t a4

MYOCAMLRUNPARAM=bp  # add p to get parse debug output

WEBROOTDIR=~/public_html/
WEBDIR=$(WEBROOTDIR)/cppmem


# commented this out as it breaks the server install
#include batch.mk # defines DOTTESTS and NEATOTESTS


all: bin/main web/cppmem.js


batch: bin/main.opt ./run.sh ./fixup.sh # $(DOTTESTS) $(NEATOTESTS)
	rm -f generated/*
	@for TEST in $(NEATOTESTS); do \
	  ./run.sh $$TEST.c neato; done
	@for TEST in $(DOTTESTS); do \
	  ./run.sh $$TEST.c dot; done



cmm.ml cmm.html: # $(ATOMICDIR)/cmm.lem  #NB this dependency is removed in the server copy
	rm -f $@
	$(LEMDIR)/lem -html -ocaml -lib $(LEMDIR)/library $(ATOMICDIR)/cmm.lem
	chmod u-w $@

cppmem bin/main bin/main.opt: cmm.ml ocamlbuild 
	rm -rf cppmem
	mkdir -p bin
	cp _build/main.d.byte bin/main
	cp _build/main.native bin/main.opt
	ln -s bin/main.opt cppmem 

OBCFLAGS=-cflags -g,-I,../$(LEMDIR)ocaml-lib/_build/
OBLFLAGS=-lflags -g,-I,../$(LEMDIR)ocaml-lib/_build/
OBXFLAGS=-X bin -X doc -X $(JSOFOCAMLDIR) -X web
OBMISCFLAGS=-no-links -classic-display

ocamlbuild: extract.cma extract.cmxa cmm.ml
	ocamlbuild -libs extract $(OBCFLAGS) $(OBLFLAGS) $(OBXFLAGS) $(OBMISCFLAGS) main.otarget


$(JSOFOCAML) :
	cd $(JSOFOCAMLDIR); make; make install

cgi/dot.cgi : cgi/dot.ml
	ocamlbuild $(OBXFLAGS) $(OBMISCFLAGS) cgi/dot.d.byte
	cp _build/cgi/dot.d.byte $@

# TODO: jp: this is not enough
web/cppmem.js : main_js.ml $(JSOFOCAML) cmm.ml
	export OCAMLPATH=$(PWD)/$(JSOFOCAMLDIR)/local-install/site-lib; \
	           ocamlbuild -use-ocamlfind -libs extract,js_of_ocaml \
                   $(OBCFLAGS) $(OBLFLAGS) $(OBXFLAGS) $(OBMISCFLAGS) \
                   main_js.d.byte
	export OCAMLPATH=$(PWD)/$(JSOFOCAMLDIR)/local-install/site-lib; \
		   $(JSOFOCAML) _build/main_js.d.byte -pretty
	cp _build/main_js.d.js $@

##########################################################

# TODO: jp: this is very ad hoc
tar : clean cmm.ml
	rm -rf cppmem # TODO: bad
	mkdir cppmem
	cp _tags cppmem
	cp render_script.sh cppmem
	cp $(ATOMICDIR)/cmm.lem cppmem
	cp *.ml cppmem
	cp *.mli cppmem
	cp *.mll cppmem
	cp *.mly cppmem
	cp Makefile cppmem
	cp main.itarget cppmem
	cd $(JSOFOCAMLDIR); make clean
	cp -R $(JSOFOCAMLDIR) cppmem/$(JSOFOCAMLDIR)
	rm -rf cppmem/$(JSOFOCAMLDIR)/local-install
	cp $(LEMDIR)/ocaml-lib/*.ml cppmem
	cp $(LEMDIR)/ocaml-lib/*.mli cppmem
	cp $(LEMDIR)/ocaml-lib/*.mllib cppmem
	mkdir cppmem/cgi
	cp cgi/*.ml cppmem/cgi
	cp cgi/*.mli cppmem/cgi
	mkdir cppmem/web
	cp cmm.html cppmem
	cp web/index.html web/help.html web/cppmem.css web/cppmem_js.css cppmem/web
	mkdir cppmem/cil-parser
	cp cil-parser/*.ml cppmem/cil-parser
	cp cil-parser/*.mli cppmem/cil-parser
	cp cil-parser/*.mll cppmem/cil-parser
	cp cil-parser/*.mly cppmem/cil-parser
	sh fixmakefilefortar.sh cppmem/Makefile
	cp -R examples cppmem 
	cp revision.txt cppmem
	cp README.txt cppmem
	cp LICENCE.txt cppmem
	cat INSTALL.txt | sed '/= From SVN =/,//d' > cppmem/INSTALL.txt
	cp server/lighttpd.conf cppmem
	tar cvzf cppmem.tar.gz cppmem


# to install the JavaScript and web page material in the local $(WEBDIR)
install-web: web/cppmem.js cgi/dot.cgi cmm.html
	rm -rf $(WEBDIR)/*
	mkdir -p $(WEBDIR)
	cp web/cppmem.js $(WEBDIR)
	cp cmm.html $(WEBDIR)
	cp web/index.html $(WEBDIR)
	cp web/help.html $(WEBDIR)
	cp web/cppmem.css $(WEBDIR)
	cp web/cppmem_js.css $(WEBDIR)
	cp cgi/dot.cgi $(WEBDIR)
	cp revision.txt $(WEBDIR)
	ocaml str.cma unix.cma makeexamplesdir.ml examples $(WEBDIR)
	#mv $(WEBDIR)/examples/examples.json $(WEBDIR)

# to build and copy the tarball to the server
install-svr:
	make tar
	scp server/install-cppmem.sh $(SVR):~
	scp cppmem.tar.gz $(SVR):~




################### headers #######################

HEADEDFILES=\
atomic.ml auxl.ml constraints.ml error.ml error.mli eval.ml execfile.ml \
execlexer.mll execparser.mly executions.ml globals.ml ids.ml ids.mli \
interact.ml iso.ml location.ml main.ml main_js.ml makeexamplesdir.ml \
pp.ml runOpsem.ml solver.ml types.ml value.ml value.mli

headers:
	echo 'Adding headers to the files...'
	headache -c cppmem_ha_cfg -h cppmem_ha_hdr $(HEADEDFILES)
	echo 'Done.'


####################################################

FORCE:
clean::
	/bin/rm -f *.byte *.native
	rm -rf out.*
	rm -rf doc.dvi doc.ps doc.pdf doc.log doc.aux
	rm -rf cmm.ml cmm.html web/cppmem.js
	rm -rf *~
	rm -rf execparser.ml execparser.output execparser.mli
	rm -rf out.* out2.* exectest.dot 
	rm -rf generated/*
	rm -rf bin/main*
	ocamlbuild -classic-display -clean
	rm -rf _build
	rm -rf cppmem
	rm -rf examples.json




extract.cma :
	ocamlbuild $(OBXFLAGS) extract.cma
extract.cmxa :
	ocamlbuild $(OBXFLAGS) extract.cmxa
