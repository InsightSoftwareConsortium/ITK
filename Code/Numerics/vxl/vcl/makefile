# This rule will not actually get updated
# until after dependencies have been made
# but that will probably be OK as the .h
# files in ./generic are now under cvs.
# The || true is to make sure the build
# doesn't fail for systems where perl is
# not available.
xall::
	( cd ./generic && $(MAKE) ) || true

LIBRARY := vcl
VERSION := 1.0

SOURCES += vcl_cmath.cxx
SOURCES += vcl_cstdlib.cxx
SOURCES += vcl_cassert.cxx
SOURCES += $(wildcard vcl_rtti.cxx)
SOURCES += $(wildcard vcl_debug.cxx)
SOURCES += $(wildcard vcl_chkr.cxx)
MANPAGE_SOURCES += vcl_algorithm.txx
MANPAGE_SOURCES += vcl_complex.txx
MANPAGE_SOURCES += vcl_deque.txx
MANPAGE_SOURCES += vcl_functional.txx
MANPAGE_SOURCES += vcl_iterator.txx
MANPAGE_SOURCES += vcl_list.txx
MANPAGE_SOURCES += vcl_map.txx
MANPAGE_SOURCES += vcl_set.txx
MANPAGE_SOURCES += vcl_stack.txx
MANPAGE_SOURCES += vcl_string.txx
MANPAGE_SOURCES += vcl_utility.txx
MANPAGE_SOURCES += vcl_vector.txx

#######################################
include ${IUEROOT}/config/top-params.mk
#######################################

# It is OK to compile these sources with native
# STLs because they won't generate any code.
ifndef USE_NATIVE_STL
SOURCES += emulation/vcl_straits.cxx
SOURCES += emulation/vcl_hashtable.cxx
SOURCES += emulation/vcl_string_instances.cxx
SOURCES += emulation/vcl_alloc.cxx
SOURCES += emulation/vcl_rbtree_instances.cxx
MANPAGE_SOURCES += emulation/vcl_algorithm.txx
MANPAGE_SOURCES += emulation/vcl_complex.txx
MANPAGE_SOURCES += emulation/vcl_deque.txx
MANPAGE_SOURCES += emulation/vcl_functional.txx
MANPAGE_SOURCES += emulation/vcl_hash_map.txx # uses emulation/vcl_hash.txx
MANPAGE_SOURCES += emulation/vcl_iterator.txx
MANPAGE_SOURCES += emulation/vcl_list.txx
MANPAGE_SOURCES += emulation/vcl_map.txx # uses emulation/vcl_rbtree.txx
MANPAGE_SOURCES += emulation/vcl_multimap.txx
MANPAGE_SOURCES += emulation/vcl_set.txx # uses emulation/vcl_rbtree.txx
MANPAGE_SOURCES += emulation/vcl_string.txx
MANPAGE_SOURCES += emulation/vcl_utility.txx
MANPAGE_SOURCES += emulation/vcl_vector.txx
endif

DEFINES += -DBUILDING_VCL_DLL

ifneq (,$(strip $(wildcard $(HOME)/fast.mk))) # fsm@robots.ox.ac.uk
VERYFAST := -O3 -ffast-math -fstrict-aliasing -fstrength-reduce -funroll-loops -fgcse  -march=pentiumpro
FLAGS_Templates/vcl_map+int.void~-.cxx += $(VERYFAST)
FLAGS_Templates/vcl_map+vcl_pair+int.int-.void~-.cxx += $(VERYFAST)
endif

ifndef NOTEST
SUBDIRS = tests
endif

######################################
include ${IUEROOT}/config/top-rules.mk
######################################

# on HP unix, this library has become too big, so it needs -fPIC
ifeq "HP-UX" "$(findstring HP-UX,$(OS))"
pic = $(PIC)
endif
