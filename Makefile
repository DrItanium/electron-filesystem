########### MAKEFILE FOR THE ADVENTURE ENGINE ###########

.PHONY: clean program 

program: 
	$(MAKE) -C src

clean: 
	$(MAKE) -C src clean 
