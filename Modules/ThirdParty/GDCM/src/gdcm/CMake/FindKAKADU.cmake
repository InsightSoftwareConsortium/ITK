#
# this module looks for KAKADu
# http://www.kakadusoftware.com/
#
#
#  Copyright (c) 2006-2010 Mathieu Malaterre <mathieu.malaterre@gmail.com>
#
#  Redistribution and use is allowed according to the terms of the New
#  BSD license.
#  For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#

FIND_PROGRAM(KDU_EXPAND_EXECUTABLE
  kdu_expand
  )

MARK_AS_ADVANCED(
  KDU_EXPAND_EXECUTABLE
  )
