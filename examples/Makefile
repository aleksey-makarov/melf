PREFIX = aarch64-unknown-linux-gnu-
CFLAGS = -Wall

EXECS   = helloWorldObj test
OBJS    =
DISASMS = $(addsuffix .dis,    $(OBJS) $(EXECS))
LAYOUTS = $(addsuffix .layout, $(OBJS) $(EXECS))
DUMPS   = $(addsuffix .dump,   $(OBJS) $(EXECS))
BUILD   = $(EXECS) $(OBJS) $(DISASMS) $(LAYOUTS) $(DUMPS)

all : $(BUILD)

helloWorldExe helloWorldObj test: LDFLAGS += -nostdlib

%.dis : %
	$(PREFIX)objdump -D $< > $@

%.layout : %
	hobjlayout $< > $@

%.dump : %
	hobjdump -f $< > $@

clean:
	rm -rf $(BUILD)
