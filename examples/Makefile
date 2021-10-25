PREFIX = aarch64-unknown-linux-gnu-
CFLAGS = -Wall

EXECS   = helloWorldObj test
SHOW    = forwardLabelExe helloWorldExe helloWorldObj.o test
DISASMS = $(addsuffix .dis,    $(SHOW))
LAYOUTS = $(addsuffix .layout, $(SHOW))
DUMPS   = $(addsuffix .dump,   $(SHOW))
BUILD   = $(EXECS) $(DISASMS) $(LAYOUTS) $(DUMPS)

all : $(BUILD)

$(EXECS) : LDFLAGS += -nostdlib

%.dis : %
	$(PREFIX)objdump -D $< > $@

%.layout : %
	hobjlayout $< > $@

%.dump : %
	hobjdump -f $< > $@

clean:
	rm -rf $(BUILD)
