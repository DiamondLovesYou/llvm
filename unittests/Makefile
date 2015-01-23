##===- unittests/Makefile ----------------------------------*- Makefile -*-===##
#
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
#
##===----------------------------------------------------------------------===##

ifeq ($(HOST_OS),NativeClient)
all:
	echo skipping

install:
	echo skipping

else

LEVEL = ..

PARALLEL_DIRS = ADT Analysis Bitcode CodeGen DebugInfo ExecutionEngine IR \
		LineEditor Linker MC Option Support Transforms

include $(LEVEL)/Makefile.config
include $(LLVM_SRC_ROOT)/unittests/Makefile.unittest

clean::
	$(Verb) $(RM) -f *Tests

endif
