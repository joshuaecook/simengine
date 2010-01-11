# Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.
include ../include/common.make

.PHONY: all
all: json

.PHONY: clean
clean:
	$(RM) json.lex.sml
	$(RM) json test

JSON_MLB = json.mlb
JSON_SOURCES := $(shell $(SMLC) -stop f $(JSON_MLB))

json: $(JSON_MLB) $(JSON_SOURCES)
	$(COMPILE.sml) -output $@ $<

TEST_MLB = test.mlb
TEST_SOURCES := $(shell $(SMLC) -stop f $(TEST_MLB))

test: $(TEST_MLB) $(TEST_SOURCES)
	$(COMPILE.sml) -output $@ $<