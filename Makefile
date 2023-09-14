%: %.rinha
	rinha $^ > "$^".json
	cargo r -- "$^".json
	cc -o $^.out output.o target/debug/librinha_rt.a 
	chmod u+x $^.out