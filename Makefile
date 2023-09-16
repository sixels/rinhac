%: %.rinha
	rinha $^ > "$^".json
	cargo r -- "$^".json
	cc  -o $^.out output.o target/debug/librinha_core.so
	chmod u+x $^.out