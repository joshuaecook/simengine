#!/bin/sh
mlton -default-ann 'allowFFI true' -export-header ffi-simengine.h main.sml ffi-simengine.c
