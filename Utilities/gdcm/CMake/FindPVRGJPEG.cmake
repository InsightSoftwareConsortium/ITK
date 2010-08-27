#
# this module looks for PVRG-JPEG
#
# PVRG_JPEG_EXECUTABLE - the full path to pvrg-jpeg
# PVRG_JPEG_FOUND      - If false, don't attempt to use pvrg-jpeg
#
#  Copyright (c) 2006-2010 Mathieu Malaterre <mathieu.malaterre@gmail.com>
#
#  Redistribution and use is allowed according to the terms of the New
#  BSD license.
#  For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#

FIND_PROGRAM(PVRGJPEG_EXECUTABLE
  pvrg-jpeg
  )


MARK_AS_ADVANCED(
  PVRGJPEG_EXECUTABLE
  )


IF (PVRGJPEG_EXECUTABLE)
    SET(PVRGJPEG_FOUND "YES")
ELSE (PVRGJPEG_EXECUTABLE)
  SET(PVRGJPEG_FOUND "NO")
ENDIF (PVRGJPEG_EXECUTABLE)

IF (PVRGJPEG_FOUND)
   IF (NOT PVRGJPEG_FIND_QUIETLY)
      MESSAGE(STATUS "Found PVRGJPEG: ${PVRGJPEG_EXECUTABLE}")
   ENDIF (NOT PVRGJPEG_FIND_QUIETLY)
ELSE (PVRGJPEG_FOUND)
   IF (PVRGJPEG_FIND_REQUIRED)
      MESSAGE(FATAL_ERROR "Could not find PVRGJPEG exe")
   ENDIF (PVRGJPEG_FIND_REQUIRED)
ENDIF (PVRGJPEG_FOUND)
