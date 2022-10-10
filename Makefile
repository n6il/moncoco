all: dsk

moncoco.bin: moncoco.asm
	lwasm -b --list=moncoco.lst -o moncoco.bin moncoco.asm

dsk: moncoco.bin
	rm -f noice.dsk
	decb dskini noice.dsk
	decb copy allram.bas noice.dsk,ALLRAM.BAS -t -0 -b
	decb copy moncoco.bin noice.dsk,MONCOCO.BIN -2 -b
	decb dir noice.dsk

clean:
	rm -f noice.dsk moncoco.bin moncoco.lst
