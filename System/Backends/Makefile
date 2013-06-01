ifndef BACKEND_DEFINED 
$(error Run make from the root of this project instead of here!) 
endif
ifndef TARGET_BACKEND
$(error Run make from the root of this project instead of here!)
endif

SUBDIRS = $(TARGET_BACKEND)

.PHONY: all subdirs $(SUBDIRS)

all: subdirs 

clean:
	for d in $(SUBDIRS); do($(MAKE) -C $$d clean ); done

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@
