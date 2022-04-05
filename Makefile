LIBS=lib/github.com/diku-dk/sorts/radix_sort.fut
FUT=futhark

.PHONY: debug build bench test


debug: $(LIBS)
	$(FUT) c src/btree.fut


build: $(LIBS)
	$(FUT) opencl src/btree.fut


bench: bench-c bench-cuda

bench-c: $(LIBS)
	$(FUT) bench --backend=c src/btree-bench.fut

bench-cuda: $(LIBS)
	$(FUT) bench --backend=cuda src/btree-bench.fut

test: $(LIBS)
	$(FUT) test --concurrency=`nproc` src/btree-test.fut


$(LIBS): futhark.pkg
	$(FUT) pkg sync
