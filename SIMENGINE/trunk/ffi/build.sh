#!/bin/sh
mlton -default-ann 'allowFFI true' -export-header ffi-simengine.h -cc-opt "-g -gdwarf-2" main.sml ffi-simengine.c
