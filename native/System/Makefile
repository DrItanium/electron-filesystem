SUBDIRS = Initialization Platform

.PHONY: all clean subdirs $(SUBDIRS)

all:
	for d in $(SUBDIRS); do($(MAKE) -C $$d); done

clean:
	for d in $(SUBDIRS); do($(MAKE) -C $$d clean ); done

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@
