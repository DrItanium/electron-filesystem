ifndef BACKEND_DEFINED
$(error Run make from the root of the project, not here!)
endif
OBJS = KeyboardImplementation.o MouseImplementation.o

.c.o :
	$(CC) -c $(CFLAGS) -std=c99 -Wall -Wundef -Wpointer-arith -Wshadow -Wcast-qual \
	    -Wcast-align -Winline -Wmissing-declarations -Wredundant-decls \
	    -Woverloaded-virtual -Wmissing-prototypes -Wnested-externs \
	    -Wstrict-prototypes -Waggregate-return -Wno-implicit -I$(srcroot) $<

.PHONY: all
all: $(OBJS)

KeyboardImplementation.o: KeyboardImplementation.c 

MouseImplementation.o: MouseImplementation.c 

.PHONY: clean
clean:
	rm -f *.o
