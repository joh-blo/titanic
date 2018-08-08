# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------
include vsn.mk
SUB_DIRECTORIES = src c_src

include ../meadow/priv/Makefile.subdir

build:
	make
