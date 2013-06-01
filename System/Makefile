ifndef BACKEND_DEFINED 
$(error Run make from the root of this project not here!) 
endif
SUBDIRS = Core Initialization Platform Input Backends
OBJS = System.o

.PHONY: all subdirs $(SUBDIRS)
.c.o :
	$(CC) -c $(CFLAGS) -std=c99 -Wall -Wundef -Wpointer-arith -Wshadow -Wcast-qual \
	    -Wcast-align -Winline -Wmissing-declarations -Wredundant-decls \
	    -Woverloaded-virtual -Wmissing-prototypes -Wnested-externs \
	    -Wstrict-prototypes -Waggregate-return -Wno-implicit -I$(srcroot) $<

all: subdirs $(OBJS)

clean:
	rm -f *.o
	for d in $(SUBDIRS); do($(MAKE) -C $$d clean ); done

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@
	
System.o: System.c System.h
