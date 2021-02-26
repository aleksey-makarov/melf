PREFIX = aarch64-unknown-linux-gnu-
CFLAGS = -Wall

all : libc.o libc.dis libc.layout

%.dis : %.o
	$(PREFIX)objdump -d $< > $@

%.layout : %.o
	hobjlayout $< > $@

clean:
	rm -rf *.o *.dis *.layout
