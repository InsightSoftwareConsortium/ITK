# - Find UUID
# Find the native UUID includes and library
# This module defines
#  UUID_INCLUDE_DIR, where to find jpeglib.h, etc.
#  UUID_LIBRARIES, the libraries needed to use UUID.
#  UUID_FOUND, If false, do not try to use UUID.
# also defined, but not for general use are
#  UUID_LIBRARY, where to find the UUID library.
#
#  Copyright (c) 2006-2010 Mathieu Malaterre <mathieu.malaterre@gmail.com>
#
#  Redistribution and use is allowed according to the terms of the New
#  BSD license.
#  For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#

# On MacOSX we have:
# $ nm -g /usr/lib/libSystem.dylib | grep uuid_generate
# 000b3aeb T _uuid_generate
# 0003e67e T _uuid_generate_random
# 000b37a1 T _uuid_generate_time
IF(APPLE)
  SET(UUID_LIBRARY_VAR System)
ELSE(APPLE)
  # Linux type:
  SET(UUID_LIBRARY_VAR uuid)
ENDIF(APPLE)

FIND_LIBRARY(UUID_LIBRARY
  NAMES ${UUID_LIBRARY_VAR}
  PATHS /lib /usr/lib /usr/local/lib
  )

# Must be *after* the lib itself
SET(CMAKE_FIND_FRAMEWORK_SAVE ${CMAKE_FIND_FRAMEWORK})
SET(CMAKE_FIND_FRAMEWORK NEVER)

FIND_PATH(UUID_INCLUDE_DIR uuid/uuid.h
/usr/local/include
/usr/include
)

IF (UUID_LIBRARY AND UUID_INCLUDE_DIR)
  SET(UUID_LIBRARIES ${UUID_LIBRARY})
  SET(UUID_FOUND "YES")
ELSE (UUID_LIBRARY AND UUID_INCLUDE_DIR)
  SET(UUID_FOUND "NO")
ENDIF (UUID_LIBRARY AND UUID_INCLUDE_DIR)


IF (UUID_FOUND)
   IF (NOT UUID_FIND_QUIETLY)
      MESSAGE(STATUS "Found UUID: ${UUID_LIBRARIES}")
   ENDIF (NOT UUID_FIND_QUIETLY)
ELSE (UUID_FOUND)
   IF (UUID_FIND_REQUIRED)
      MESSAGE( "library: ${UUID_LIBRARY}" )
      MESSAGE( "include: ${UUID_INCLUDE_DIR}" )
      MESSAGE(FATAL_ERROR "Could not find UUID library")
   ENDIF (UUID_FIND_REQUIRED)
ENDIF (UUID_FOUND)

# Deprecated declarations.
#SET (NATIVE_UUID_INCLUDE_PATH ${UUID_INCLUDE_DIR} )
#GET_FILENAME_COMPONENT (NATIVE_UUID_LIB_PATH ${UUID_LIBRARY} PATH)

MARK_AS_ADVANCED(
  UUID_LIBRARY
  UUID_INCLUDE_DIR
  )
SET(CMAKE_FIND_FRAMEWORK ${CMAKE_FIND_FRAMEWORK_SAVE})
