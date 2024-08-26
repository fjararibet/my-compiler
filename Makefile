%.run: %.o
	clang -g -m64 -o $@ main.c $<

%.o: %.s
	nasm -f elf64 -o $@ $<

%.s: %.int
	dune exec ./bin/main.exe $< > $@

runtests:
	dune exec ./test/test_compiler.exe

clean:
	rm -rf _build
	rm *.run
