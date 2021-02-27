PREFIX = aarch64-unknown-linux-gnu-
CFLAGS = -Wall

all : libc.o libc.dis libc.layout libc.dump

%.dis : %.o
	$(PREFIX)objdump -d $< > $@

%.layout : %.o
	hobjlayout $< > $@

%.dump : %.o
	hobjdump $< > $@

clean:
	rm -rf *.o *.dis *.layout *.dump
