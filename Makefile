########### MAKEFILE FOR THE ADVENTURE ENGINE ###########
export progroot ?= $(CURDIR)
export srcroot := $(progroot)/native/

.PHONY: clean program 

program: 
	$(MAKE) -C native

clean: 
	$(MAKE) -C native clean 
