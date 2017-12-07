main: FORCE
	stack ghc -- -c app/Main.hs
	cc -c app/Main.c -o lib/Main_c.o
	stack ghc -- app/Main.o lib/Main_c.o -lm -o app/Main_exe
	# ./app/Main_exe    # <-- run

clean:
	rm app/*.o app/*.hi app/*.c app/Main_exe

clean-lib:
	lib/*.o




FORCE:
