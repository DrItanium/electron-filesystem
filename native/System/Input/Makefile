OBJS = MouseInput.o KeyboardInput.o Input.o

.c.o :
	$(CC) -c $(CFLAGS) -std=c99 -Wall -Wundef -Wpointer-arith -Wshadow -Wcast-qual \
	    -Wcast-align -Winline -Wmissing-declarations -Wredundant-decls \
	    -Woverloaded-virtual -Wmissing-prototypes -Wnested-externs \
	    -Wstrict-prototypes -Waggregate-return -Wno-implicit -I$(srcroot) $<

.PHONY: all
all: $(OBJS)


MouseInput.o: MouseInput.c MouseInput.h 

KeyboardInput.o: KeyboardInput.c KeyboardInput.h 

Input.o: Input.c Input.h

.PHONY: clean
clean:
	rm -f *.o
