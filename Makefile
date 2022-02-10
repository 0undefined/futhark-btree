CC=futhark

debug:
	$(CC) c src/btree.fut

build:
	$(CC) opencl src/btree.fut
