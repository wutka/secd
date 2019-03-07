secd: secd.h secd.c secd_linux.c
	gcc -DDEBUG -o secd secd.c secd_linux.c
