# 
# this module looks for Dicom3Tools, well right now only dciodvfy
#
# DCIODVFY_EXECUTABLE - the full path to the dciodvfy
# DCIODVFY_FOUND      - If false, don't attempt to use dciodvfy

FIND_PROGRAM(DCIODVFY_EXECUTABLE
  dciodvfy
  "/tmp/"
  "/tmp/dicom3tools_1.00.snapshot.20041227.graymax/appsrc/dcfile/"
  "${DICOM3TOOLS}/bin"
  )

MARK_AS_ADVANCED(
  DCIODVFY_EXECUTABLE
  )

IF (NOT DCIODVFY_EXECUTABLE)
  SET(DCIODVFY_FOUND "NO")
ELSE (NOT DCIODVFY_EXECUTABLE)
  SET(DCIODVFY_FOUND "YES")
ENDIF (NOT DCIODVFY_EXECUTABLE)

