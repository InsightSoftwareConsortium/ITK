#
# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
#
# HDF5 High-Level Library Makefile(.in)
#

include $(top_srcdir)/config/commence.am
include $(top_srcdir)/config/lt_vers.am

# Add include directories to the C preprocessor flags
AM_CPPFLAGS+=-I$(top_srcdir)/src

# This library is our main target.
lib_LTLIBRARIES=libhdf5_hl.la

# Add libtool numbers to the HDF5 hl library (from config/lt_vers.am)
libhdf5_hl_la_LDFLAGS= -version-info $(LT_HL_VERS_INTERFACE):$(LT_HL_VERS_REVISION):$(LT_HL_VERS_AGE) $(AM_LDFLAGS)

# List sources to include in the HDF5 HL Library.
libhdf5_hl_la_SOURCES=H5DO.c H5DS.c H5IM.c H5LT.c H5LTanalyze.c H5LTparse.c H5PT.c H5TB.c H5LD.c

# HDF5 HL library depends on HDF5 Library.
libhdf5_hl_la_LIBADD=$(LIBHDF5)

# Public header files (to be installed)
include_HEADERS=hdf5_hl.h H5DOpublic.h H5IMpublic.h H5LTpublic.h H5TBpublic.h H5DSpublic.h H5PTpublic.h H5LDpublic.h

include $(top_srcdir)/config/conclude.am
