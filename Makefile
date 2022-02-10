FUTC=futhark

.PHONY: pkg

debug: pkg
	$(FUTC) c src/btree.fut

build: pkg
	$(FUTC) opencl src/btree.fut

pkg: lib/github.com/diku-dk/sorts/radix_sort.fut
	$(FUTC) pkg sync

lib/github.com/diku-dk/sorts/radix_sort.fut: futhark.pkg

futhark.pkg:
	$(FUTC) pkg add github.com/diku-dk/sorts
