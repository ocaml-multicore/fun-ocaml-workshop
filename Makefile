all: 
	dune build	@install

tracer:
	dune exec ./bin/tracer/main.exe