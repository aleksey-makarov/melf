PREFIX = aarch64-unknown-linux-gnu-
CFLAGS = -Wall

EXECS   = syscalls obj
OBJS    = syscalls.o objs.o
DISASMS = $(addsuffix .dis,    $(OBJS) $(EXECS))
LAYOUTS = $(addsuffix .layout, $(OBJS) $(EXECS))
DUMPS   = $(addsuffix .dump,   $(OBJS) $(EXECS))
BUILD   = $(EXECS) $(OBJS) $(DISASMS) $(LAYOUTS) $(DUMPS)

all : $(BUILD)

obj syscalls: LDFLAGS += -nostdlib
# -Wl,-Ttext-segment=0x40ffc8

%.dis : %
	$(PREFIX)objdump -D $< > $@

%.layout : %
	hobjlayout $< > $@

%.dump : %
	hobjdump -f $< > $@

clean:
	rm -rf $(BUILD)
