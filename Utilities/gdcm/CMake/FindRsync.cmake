#
# This module finds if rsync is installed
#
#  RSYNC_EXECUTABLE         = full path to the pike binary
#
# Typical usage for gdcm is:
# rsync -avH --delete [options] rsync.creatis.insa-lyon.fr::module localdir
# Compression option is: -z
#
#  Copyright (c) 2006-2010 Mathieu Malaterre <mathieu.malaterre@gmail.com>
#
#  Redistribution and use is allowed according to the terms of the New
#  BSD license.
#  For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#

FIND_PROGRAM(RSYNC_EXECUTABLE
  NAMES rsync
  PATHS
  /usr/bin
  /usr/local/bin
  )

MARK_AS_ADVANCED(
  RSYNC_EXECUTABLE
  )
