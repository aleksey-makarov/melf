PREFIX = aarch64-unknown-linux-gnu-
CFLAGS = -Wall

EXECS   = libc syscallc
OBJS    = libc.o syscallc.o lib_main.o
DISASMS = $(addsuffix .dis,    $(OBJS) $(EXECS))
LAYOUTS = $(addsuffix .layout, $(OBJS) $(EXECS))
DUMPS   = $(addsuffix .dump,   $(OBJS) $(EXECS))
BUILD   = $(EXECS) $(OBJS) $(DISASMS) $(LAYOUTS) $(DUMPS)

all : $(BUILD)

libc: libc.o lib_main.o
syscallc: LDFLAGS += -nostdlib

%.dis : %
	$(PREFIX)objdump -d $< > $@

%.layout : %
	hobjlayout $< > $@

%.dump : %
	hobjdump $< > $@

clean:
	rm -rf $(BUILD)
