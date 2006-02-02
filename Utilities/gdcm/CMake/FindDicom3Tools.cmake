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

FIND_PROGRAM(DCIODVFY_EXECUTABLE
  dciodvfy
  "/tmp/"
  "$ENV{DICOM3TOOLS}/bin"
  "$ENV{DICOM3TOOLS}/bin/1.2.6.8."
  )

MARK_AS_ADVANCED(
  DCIODVFY_EXECUTABLE
  )

IF (NOT DCIODVFY_EXECUTABLE)
  SET(DCIODVFY_FOUND "NO")
ELSE (NOT DCIODVFY_EXECUTABLE)
  SET(DCIODVFY_FOUND "YES")
ENDIF (NOT DCIODVFY_EXECUTABLE)

