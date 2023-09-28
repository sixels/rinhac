.PHONY: core
core:
	$(CC) -c -fPIC ./core/core.c -o ./core/core.o
	$(CC) -shared ./core/core.o -o ./core/librinha_core.so

%: %.rinha
	rinha $^ > "$^".json
	cargo r -- "$^".json