.PHONY: core
core:
	$(CC) -c -fPIC ./core/core.c -o ./core/core.o
	$(CC) -shared ./core/core.o -o ./core/librinha_core.so

%: %.rinha
	rinha $^ > "$^".json
	cargo r -- "$^".json
	$(CC)  -o $^.out output.o ./core/librinha_core.so
	chmod u+x $^.out