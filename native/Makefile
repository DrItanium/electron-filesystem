########### MAKEFILE FOR THE ADVENTURE ENGINE ###########
export progroot ?= $(CURDIR)/../
export srcroot ?= $(progroot)/native/
#########################################################
# While I don't like gcc as much as clang, I realize that 
# gcc is more prevalent these days
#########################################################
CC = gcc 
OUTPUT = adventure-engine 
SUBDIRS = Core System
OBJS = Core/agenda.o Core/analysis.o Core/argacces.o Core/bload.o Core/bmathfun.o Core/bsave.o \
 	Core/classcom.o Core/classexm.o Core/classfun.o Core/classinf.o Core/classini.o \
	Core/classpsr.o Core/clsltpsr.o Core/commline.o Core/conscomp.o Core/constrct.o \
 	Core/constrnt.o Core/crstrtgy.o Core/cstrcbin.o Core/cstrccom.o Core/cstrcpsr.o \
 	Core/cstrnbin.o Core/cstrnchk.o Core/cstrncmp.o Core/cstrnops.o Core/cstrnpsr.o \
 	Core/cstrnutl.o Core/default.o Core/defins.o Core/developr.o Core/dffctbin.o Core/dffctbsc.o \
 	Core/dffctcmp.o Core/dffctdef.o Core/dffctpsr.o Core/dffnxbin.o Core/dffnxcmp.o \
	Core/dffnxexe.o Core/dffnxfun.o Core/dffnxpsr.o Core/dfinsbin.o Core/dfinscmp.o Core/drive.o \
	Core/emathfun.o \
 	Core/engine.o Core/envrnmnt.o Core/evaluatn.o Core/expressn.o Core/exprnbin.o Core/exprnops.o \
 	Core/exprnpsr.o Core/extnfunc.o Core/factbin.o Core/factbld.o Core/factcmp.o Core/factcom.o \
 	Core/factfun.o Core/factgen.o Core/facthsh.o Core/factlhs.o Core/factmch.o Core/factmngr.o \
 	Core/factprt.o Core/factqpsr.o Core/factqury.o Core/factrete.o Core/factrhs.o Core/filecom.o \
 	Core/filertr.o Core/generate.o Core/genrcbin.o Core/genrccmp.o Core/genrccom.o Core/genrcexe.o \
 	Core/genrcfun.o Core/genrcpsr.o Core/globlbin.o Core/globlbsc.o Core/globlcmp.o Core/globlcom.o \
 	Core/globldef.o Core/globlpsr.o Core/immthpsr.o Core/incrrset.o Core/inherpsr.o \
 	Core/inscom.o Core/insfile.o Core/insfun.o Core/insmngr.o Core/insmoddp.o Core/insmult.o \
 	Core/inspsr.o Core/insquery.o Core/insqypsr.o Core/iofun.o Core/lgcldpnd.o \
 	Core/memalloc.o Core/miscfun.o Core/modulbin.o Core/modulbsc.o Core/modulcmp.o Core/moduldef.o \
 	Core/modulpsr.o Core/modulutl.o Core/msgcom.o Core/msgfun.o Core/msgpass.o Core/msgpsr.o \
 	Core/multifld.o Core/multifun.o Core/objbin.o Core/objcmp.o Core/objrtbin.o Core/objrtbld.o \
 	Core/objrtcmp.o Core/objrtfnx.o Core/objrtgen.o Core/objrtmch.o Core/parsefun.o Core/pattern.o \
 	Core/pprint.o Core/prccode.o Core/prcdrfun.o Core/prcdrpsr.o Core/prdctfun.o Core/prntutil.o \
 	Core/proflfun.o Core/reorder.o Core/reteutil.o Core/retract.o Core/router.o Core/rulebin.o \
 	Core/rulebld.o Core/rulebsc.o Core/rulecmp.o Core/rulecom.o Core/rulecstr.o Core/ruledef.o \
 	Core/ruledlt.o Core/rulelhs.o Core/rulepsr.o Core/scanner.o Core/sortfun.o Core/strngfun.o \
 	Core/strngrtr.o Core/symblbin.o Core/symblcmp.o Core/symbol.o Core/sysdep.o Core/textpro.o \
 	Core/tmpltbin.o Core/tmpltbsc.o Core/tmpltcmp.o Core/tmpltdef.o Core/tmpltfun.o Core/tmpltlhs.o \
 	Core/tmpltpsr.o Core/tmpltrhs.o Core/tmpltutl.o Core/userdata.o Core/userfunctions.o \
 	Core/utility.o Core/watch.o Core/main.o Core/binary_operations.o System/Platform/ArchitectureDetection.o \
	System/Platform/OSDetection.o System/Platform/HardwareDetection.o System/Platform/Platform.o \
	System/Initialization/AdventureEngineInit.o System/System.o

program: subdirs 
	$(CC) $(CFLAGS) -o $(OUTPUT) $(OBJS) -lm -lncurses


.PHONY: clean sub-clean subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean: sub-clean
	rm -f $(OUTPUT)

sub-clean:
	for d in $(SUBDIRS); do ($(MAKE) -C $$d clean ); done
