# We need to make this as simple as possible
# Add folders that contain makefiles here
SUBDIRS = Core Platform 
# Add files that are meant to be compiled and are in this level of the project
# here
OBJS = System.o

.PHONY: all subdirs $(SUBDIRS)
.c.o :
	$(CC) -c $(CFLAGS) -std=c99 -Wall -Wundef -Wpointer-arith -Wshadow -Wcast-qual \
	    -Wcast-align -Winline -Wmissing-declarations -Wredundant-decls \
	    -Wmissing-prototypes -Wnested-externs \
	    -Wstrict-prototypes -Waggregate-return -Wno-implicit -I$(srcroot) $<

all: subdirs $(OBJS)

clean:
	rm -f *.o
	for d in $(SUBDIRS); do($(MAKE) -C $$d clean ); done

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@
	
System.o: System.c System.h
