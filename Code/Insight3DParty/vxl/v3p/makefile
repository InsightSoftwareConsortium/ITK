#######################################
include ${IUEROOT}/config/top-params.mk
#######################################

SUBDIRS += netlib

SUBDIRS += Qv

# do not compile zlib, png, jpeg or tiff by default as they
# are "native" libraries on some systems. clients who wish
# to compile the 3rd libraries which come with the v3p source
# distribution must specify include paths to the v3p versions.
# it will be detected here.

ifneq (,$(strip $(wildcard $(ZLIB_INC_DIR)/../v3p_tag)))
SUBDIRS += zlib
endif

ifneq (,$(strip $(wildcard $(PNG_INC_DIR)/../v3p_tag)))
SUBDIRS += png
endif

ifneq (,$(strip $(wildcard $(JPEG_INC_DIR)/../v3p_tag)))
SUBDIRS += jpeg
endif

ifneq (,$(strip $(wildcard $(TIFF_INC_DIR)/../v3p_tag)))
SUBDIRS += tiff
endif

######################################
include ${IUEROOT}/config/top-rules.mk
######################################
