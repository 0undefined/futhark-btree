LIBS=lib/github.com/diku-dk/sorts/radix_sort.fut
FUT=futhark

.PHONY: debug build


debug: $(LIBS)
	$(FUT) c src/btree.fut


build: $(LIBS)
	$(FUT) opencl src/btree.fut


bench: $(LIBS)
	@echo '# C'
	$(FUT) bench --backend=c src/btree-bench.fut
	@echo '# Cuda'
	$(FUT) bench --backend=cuda src/btree-bench.fut


test: $(LIBS)
	$(FUT) test --concurrency=`nproc` src/btree-test.fut


$(LIBS): futhark.pkg
	$(FUT) pkg sync
