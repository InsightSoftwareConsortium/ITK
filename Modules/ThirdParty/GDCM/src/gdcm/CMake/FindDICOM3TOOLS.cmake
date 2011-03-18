#
# this module looks for Dicom3Tools, well right now only dciodvfy
#
# DCIODVFY_EXECUTABLE - the full path to the dciodvfy
# DCIODVFY_FOUND      - If false, don't attempt to use dciodvfy

# dicom3tools are funny to build you'll need imake
# Anyway in order not to pollute your system, you can do an in-source build
# and install which should be clean enough:
#
# ./Configure
# imake -I./config -DInstallInTopDir
# make World
# make install (will copy in ./bin)
#
# then all you need to do is export an env var DICOM3TOOLS pointing to that dir
#
#  Copyright (c) 2006-2010 Mathieu Malaterre <mathieu.malaterre@gmail.com>
#
#  Redistribution and use is allowed according to the terms of the New
#  BSD license.
#  For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#

FIND_PROGRAM(DCIODVFY_EXECUTABLE
  dciodvfy
  "/tmp/"
  "$ENV{DICOM3TOOLS}/bin"
  "$ENV{DICOM3TOOLS}/bin/1.2.6.8."
  )

FIND_PROGRAM(DCDUMP_EXECUTABLE
  dcdump
  "/tmp/"
  "$ENV{DICOM3TOOLS}/bin"
  "$ENV{DICOM3TOOLS}/bin/1.2.6.8."
  )


MARK_AS_ADVANCED(
  DCIODVFY_EXECUTABLE
  DCDUMP_EXECUTABLE
  )

#IF (NOT DCIODVFY_EXECUTABLE)
#  SET(DCIODVFY_FOUND "NO")
#ELSE (NOT DCIODVFY_EXECUTABLE)
#  SET(DCIODVFY_FOUND "YES")
#ENDIF (NOT DCIODVFY_EXECUTABLE)
