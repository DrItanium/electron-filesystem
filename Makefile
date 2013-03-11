########### MAKEFILE FOR THE ADVENTURE ENGINE ###########

.PHONY: clean program 

program: 
	$(MAKE) -C native

clean: 
	$(MAKE) -C native clean 
