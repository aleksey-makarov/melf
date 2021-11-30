PREFIX = aarch64-unknown-linux-gnu-
CFLAGS = -Wall
CC = $(PREFIX)gcc

EXECS   = helloWorldObj test
DOCS    = README_ru.html
SHOW    = forwardLabelExe helloWorldExe helloWorldObj.o test
DISASMS = $(addsuffix .dis,    $(SHOW))
LAYOUTS = $(addsuffix .layout, $(SHOW))
DUMPS   = $(addsuffix .dump,   $(SHOW))
BUILD   = $(EXECS) $(DOCS) $(DISASMS) $(LAYOUTS) $(DUMPS)

all : $(BUILD)

$(EXECS) : LDFLAGS += -nostdlib

%.dis : %
	$(PREFIX)objdump -D $< > $@

%.layout : %
	hobjlayout $< > $@

%.dump : %
	hobjdump -f $< > $@

# https://pandoc.org/MANUAL.html
%.html : %.md
	pandoc --standalone -o $@ $<

clean:
	rm -rf $(BUILD)
