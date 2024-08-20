%.run: %.o
	clang -g -m64 -o $@ main.c $<

%.o: %.s
	nasm -f elf64 -o $@ $<

%.s: %.int
	dune exec ./bin/compile.exe $< > $@

clean:
	rm *.run *.o *.s *.out
