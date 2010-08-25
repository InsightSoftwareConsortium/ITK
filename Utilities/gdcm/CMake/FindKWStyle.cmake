#
# this module looks for KWStyle
# http://public.kitware.com/KWStyle
#
#
#  Copyright (c) 2009-2010 Mathieu Malaterre <mathieu.malaterre@gmail.com>
#
#  Redistribution and use is allowed according to the terms of the New
#  BSD license.
#  For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#

find_program(KWSTYLE_EXECUTABLE
  NAMES KWStyle
  PATHS
  "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Kitware Inc.\\KWStyle 1.0.0]/bin"
  )

#  OPTION(KWSTYLE_USE_VIM_FORMAT "Set KWStyle to generate errors with a VIM-compatible format." OFF)
#  OPTION(KWSTYLE_USE_MSVC_FORMAT "Set KWStyle to generate errors with a VisualStudio-compatible format." OFF)
#  MARK_AS_ADVANCED(KWSTYLE_USE_VIM_FORMAT)
#  MARK_AS_ADVANCED(KWSTYLE_USE_MSVC_FORMAT)
#
#  IF(KWSTYLE_USE_VIM_FORMAT)
#    SET(KWSTYLE_ARGUMENTS -vim ${KWSTYLE_ARGUMENTS})
#  ENDIF(KWSTYLE_USE_VIM_FORMAT)
#
#  IF(KWSTYLE_USE_MSVC_FORMAT)
#    SET(KWSTYLE_ARGUMENTS -msvc ${KWSTYLE_ARGUMENTS})
#  ENDIF(KWSTYLE_USE_MSVC_FORMAT)


mark_as_advanced(
  KWSTYLE_EXECUTABLE
  )
