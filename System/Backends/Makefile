ifndef BACKEND_DEFINED 
$(error No backend defined. Run make at the root of the project)
endif

ifndef TARGET_BACKEND
$(error No backend defined. Run make at the root of the project)
endif

SUBDIRS = $(TARGET_BACKEND)

.PHONY: all subdirs $(SUBDIRS)

all: subdirs 

clean:
	for d in $(SUBDIRS); do($(MAKE) -C $$d clean ); done

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@
