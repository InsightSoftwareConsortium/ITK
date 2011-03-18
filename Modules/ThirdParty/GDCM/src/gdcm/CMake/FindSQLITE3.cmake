#
#  Copyright (c) 2006-2010 Mathieu Malaterre <mathieu.malaterre@gmail.com>
#
#  Redistribution and use is allowed according to the terms of the New
#  BSD license.
#  For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#

FIND_PATH(SQLITE3_INCLUDE_DIR sqlite3.h)

FIND_LIBRARY(SQLITE3_LIBRARY NAMES sqlite3)

INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(SQLITE3 DEFAULT_MSG SQLITE3_LIBRARY SQLITE3_INCLUDE_DIR)

SET(SQLITE3_LIBRARIES ${SQLITE3_LIBRARY})
SET(SQLITE3_INCLUDE_DIRS ${SQLITE3_INCLUDE_DIR})

MARK_AS_ADVANCED(SQLITE3_LIBRARY SQLITE3_INCLUDE_DIR )
