########### LINUX MAKEFILE FOR ISA-CLIPS ###########
### FLAGS ###
# CFLAGS - additional cflags to pass to the compiler
####################################################
OBJS = SDLInterface.o

.c.o :
	$(CC) -c $(CFLAGS) -std=c99 -Wall -Wundef -Wpointer-arith -Wshadow -Wcast-qual \
	    -Wcast-align -Winline -Wmissing-declarations -Wredundant-decls \
	    -Woverloaded-virtual -Wmissing-prototypes -Wnested-externs \
	    -Wstrict-prototypes -Waggregate-return -Wno-implicit $<

.PHONY: all
all: $(OBJS)

SDLInterface.o: SDLInterface.c SDLAudio.h SDLInterface.h

SDLAudio.o: SDLAudio.c SDLAudio.h

.PHONY: clean
clean:
	rm -f *.o
