PREFIX = aarch64-unknown-linux-gnu-
CFLAGS = -Wall
LDFLAGS = -nostdlib
CC = $(PREFIX)gcc
OBJDUMP=$(PREFIX)objdump

DOCS    = README_ru.html
OBJS    = helloWorldObj test test.o test.x86 test.x86.o
SHOW    = forwardLabelExe helloWorldExe helloWorldObj.o test test.x86
DISASMS = $(addsuffix .dis,    $(SHOW))
LAYOUTS = $(addsuffix .layout, $(SHOW))
DUMPS   = $(addsuffix .dump,   $(SHOW))
BUILD   = $(OBJS) $(DOCS) $(DISASMS) $(LAYOUTS) $(DUMPS)

all : $(BUILD)

forwardLabelExe helloWorldExe helloWorldObj.o :
	$(error "for forwardLabelExe helloWorldExe and helloWorldObj.o run tests")


%.x86 : CC=gcc
%.x86.dis : OBJDUMP=objdump

%.dis : %
	$(OBJDUMP) -D $< > $@

%.layout : %
	hobjlayout $< > $@

%.dump : %
	hobjdump -f $< > $@

# https://pandoc.org/MANUAL.html
%.html : %.md
	pandoc --standalone -o $@ $<

clean:
	rm -rf $(BUILD)
