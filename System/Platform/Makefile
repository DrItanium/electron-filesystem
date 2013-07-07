########### LINUX MAKEFILE FOR ISA-CLIPS ###########
### FLAGS ###
# CFLAGS - additional cflags to pass to the compiler
####################################################
OBJS = ArchitectureDetection.o OSDetection.o \
		 HardwareDetection.o Platform.o

.c.o :
	$(CC) -c $(CFLAGS) -std=c99 -Wall -Wundef -Wpointer-arith -Wshadow -Wcast-qual \
	    -Wcast-align -Winline -Wmissing-declarations -Wredundant-decls \
	    -Wmissing-prototypes -Wnested-externs \
	    -Wstrict-prototypes -Waggregate-return -Wno-implicit -I$(srcroot) $<

.PHONY: all
all: $(OBJS)

ArchitectureDetection.o: ArchitectureDetection.c ArchitectureDetection.h

OSDetection.o: OSDetection.c OSDetection.h

HardwareDetection.o: HardwareDetection.c HardwareDetection.h

Platform.o: Platform.c Platform.h

.PHONY: clean
clean:
	rm -f *.o
