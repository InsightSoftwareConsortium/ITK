#
# Find the native FLTK includes and library
#
# This module defines includes the CMake module that search for FLTK
# if FLTK is found the following variable is set. This variable is used
# as flag to enable the built of code that relies on FLTK.
#
# HAS_FLTK          - Final flag to be checked by other code that relies 
#                     on FLTK being available.  
#

#
# This CMake module will look for FLTK components.
# 
INCLUDE (${CMAKE_ROOT}/Modules/FindFLTK.cmake)


#
#  This is the final flag that will be checked by
#  other code that requires FLTK for compile/run.
#
IF(FLTK_FLUID_EXE)
  IF(FLTK_INCLUDE_PATH)
    IF(FLTK_LIBRARY)
      SET (HAS_FLTK 1 CACHE INTERNAL "FLTK library, headers and Fluid are available")
    ENDIF(FLTK_LIBRARY)
  ENDIF(FLTK_INCLUDE_PATH)
ENDIF(FLTK_FLUID_EXE)



MARK_AS_ADVANCED( CLEAR 
  FLTK_FLUID_EXE
  FLTK_INCLUDE_PATH
  FLTK_LIBRARY
)

