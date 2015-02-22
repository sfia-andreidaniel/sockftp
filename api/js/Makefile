.PHONY: build

SHELL := /bin/bash

build:: clean
	tsc main.ts --target ES5 --out SockFTP.js

clean::
	rm -f *.js