include ../Makefile.demo

.PHONY: bench
bench:
	dune exec ./main.exe -- time alloc samples -quota 10

.PHONY: once
once:
	dune exec ./main.exe -- time alloc -quota 1x
