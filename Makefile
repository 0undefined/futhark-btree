LIBS=lib/github.com/diku-dk/sorts/radix_sort.fut

.PHONY: debug build

debug: $(LIBS)
	futhark c src/btree.fut

build: $(LIBS)
	futhark opencl src/btree.fut

$(LIBS): futhark.pkg
	futhark pkg sync
