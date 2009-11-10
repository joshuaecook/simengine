


all: simex

simex: simex.c
	gcc -g -I../../codegen/src/simengine -o $@ $< -ldl

clean:
	rm -rf simex simex.dSYM
