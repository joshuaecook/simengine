


all: simex

simex: simex.c
	gcc -g -std=c99 -I../../codegen/src/simengine -o $@ $< -ldl

clean:
	rm -rf simex simex.dSYM
