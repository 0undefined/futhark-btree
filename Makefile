LIBS=lib/github.com/diku-dk/sorts/radix_sort.fut

.PHONY: debug build

debug: $(LIBS)
	futhark c src/btree.fut

build: $(LIBS)
	futhark opencl src/btree.fut

bench: $(LIBS)
	@echo '# C'
	futhark bench --backend=c src/btree-bench.fut
	@echo '# Cuda'
	futhark bench --backend=cuda src/btree-bench.fut

test: $(LIBS)
	futhark test --concurrency=`nproc` src/btree-test.fut


$(LIBS): futhark.pkg
	futhark pkg sync
