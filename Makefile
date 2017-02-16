
EXTERNAL_LUA_V = 5.3.4

# Define VPATH
VPATH ?= $(shell pwd)

# the default target
.PHONY: default
default: lib

# the all target
.PHONY: all static
all: static


# SMEKASETTINGS (DO NOT DELETE)
# DO NOT CHANGE CONTENT IN THIS BLOCK
# IT MAY BE OVERWRITTEN WHEN REINSTALLING SMEKA
#
# This Makefile was created by smeka:
#  github.com/zerothi/smeka

# Top-directory of Makefile/source tree
# If need set, do so ABOVE this block!
TOP_DIR ?= .

# Directory of smeka default Makefiles
SMEKA_DIR = smeka

# Include the smeka settings!
include $(TOP_DIR)/$(SMEKA_DIR)/Makefile.smeka

# SMEKAENDSETTINGS (DO NOT DELETE)


# The linker is a fortran compiler
LINK := $(FC)

# Create targets for the library
.PHONY: lib
lib: $(LIBRARIES)

# Determine whether the user has added an external
# Lua header file.
ifdef LUA_DIR
 # User-defined
 INCLUDES += -I$(LUA_DIR)/include
else
 # lua-sources shipped
 LUA_DIR = $(TOP_DIR)/external/lua-$(EXTERNAL_LUA_V)
 INCLUDES += -I$(LUA_DIR)/src
endif



# Add the LuaFortran files
include $(TOP_DIR)/LuaFortran/Makefile.inc

# Add source files
include $(TOP_DIR)/source/Makefile.inc
# Add source/extdouble files
include $(TOP_DIR)/source/extdouble/Makefile.inc
# Add source/quadruple files
include $(TOP_DIR)/source/quadruple/Makefile.inc


# Ensure that all objects are required for the libraries
$(LIBRARIES): $(OBJECTS)
