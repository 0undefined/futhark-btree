LIBS=lib/github.com/diku-dk/sorts/radix_sort.fut
FUT=futhark
BACKEND=c
BENCHPROG=src/btree-bench.fut

.PHONY: debug build bench test


debug: $(LIBS)
	$(FUT) c src/btree.fut


build: $(LIBS)
	$(FUT) opencl src/btree.fut


bench: bench-c bench-multicore bench-opencl bench-cuda

# TODO: Make use of `$(BACKEND)` and a default bench-% target
bench-c: $(LIBS)
	$(FUT) bench --runs=50 --backend=c $(BENCHPROG)

bench-multicore: $(LIBS)
	$(FUT) bench --runs=50 --backend=multicore $(BENCHPROG)

bench-cuda: $(LIBS)
	$(FUT) bench --runs=50 --backend=cuda $(BENCHPROG)

bench-opencl: $(LIBS)
	$(FUT) bench --runs=50 --backend=opencl $(BENCHPROG)

test: $(LIBS)
	$(FUT) test --concurrency=`nproc` src/btree-test.fut


$(LIBS): futhark.pkg
	$(FUT) pkg sync
