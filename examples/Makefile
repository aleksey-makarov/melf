PREFIX = aarch64-unknown-linux-gnu-
CFLAGS = -Wall

EXECS   = objc syscallc
OBJS    = objc.o syscallc.o obj_main.o
DISASMS = $(addsuffix .dis,    $(OBJS) $(EXECS))
LAYOUTS = $(addsuffix .layout, $(OBJS) $(EXECS))
DUMPS   = $(addsuffix .dump,   $(OBJS) $(EXECS))
BUILD   = $(EXECS) $(OBJS) $(DISASMS) $(LAYOUTS) $(DUMPS)

all : $(BUILD)

objc: objc.o obj_main.o
syscallc: LDFLAGS += -nostdlib

%.dis : %
	$(PREFIX)objdump -d $< > $@

%.layout : %
	hobjlayout $< > $@

%.dump : %
	hobjdump $< > $@

clean:
	rm -rf $(BUILD)
