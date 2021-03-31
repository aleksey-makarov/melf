PREFIX = aarch64-unknown-linux-gnu-
CFLAGS = -Wall

EXECS   = objc syscalls
OBJS    = objc.o syscalls.o obj_main.o objs.o
DISASMS = $(addsuffix .dis,    $(OBJS) $(EXECS))
LAYOUTS = $(addsuffix .layout, $(OBJS) $(EXECS))
DUMPS   = $(addsuffix .dump,   $(OBJS) $(EXECS))
BUILD   = $(EXECS) $(OBJS) $(DISASMS) $(LAYOUTS) $(DUMPS)

all : $(BUILD)

objc: objc.o obj_main.o
syscalls: LDFLAGS += -nostdlib

%.dis : %
	$(PREFIX)objdump -D $< > $@

%.layout : %
	hobjlayout $< > $@

%.dump : %
	hobjdump -f $< > $@

clean:
	rm -rf $(BUILD)
