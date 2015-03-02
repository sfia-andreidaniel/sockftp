.PHONY: build

SHELL := /bin/bash
OPTIMIZE ?= -O3
LINKING ?= -XX
INCLUDES := -Fuinc/indy -Fuinc/lib -Fuserver

build:: clean
	fpc sockftpd.pas -vwen -Mobjfpc $(OPTIMIZE) $(LINKING) $(INCLUDES)

clean::
	rm -f inc/lib/*.o server/*.o *.o inc/indy/*.o \
	inc/lib/*.ppu inc/indy/*.ppu server/*.ppu *.ppu \
	sockftpd sockftpd.exe