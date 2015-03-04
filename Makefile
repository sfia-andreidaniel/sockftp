.PHONY: build

SHELL := /bin/bash
OPTIMIZE ?= -O3
LINKING ?= -XX
INCLUDES := -Fuinc/indy -Fuinc/lib -Fuserver
LAZARUS ?= "c:/lazarus/lazbuild.exe"
INNO ?= echo

build:: clean
	fpc sockftpd.pas -vwen -Mobjfpc $(OPTIMIZE) $(LINKING) $(INCLUDES)
	$(LAZARUS) --build-all configurator/config.lpr
	$(INNO) /cc install\setup.iss
	

clean::
	rm -f inc/lib/*.o server/*.o *.o inc/indy/*.o \
	inc/lib/*.ppu inc/indy/*.ppu server/*.ppu *.ppu \
	sockftpd sockftpd.exe sockftpdctl.exe sockftpdctl sockftpdctl.o \
	sockftpdsvc.ini sockftpd.o install/*.msi config config.exe \
	install\setup-sockftpd.exe