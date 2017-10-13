#
# This module install PDB files.
#
# Based on users posts:
# http://www.cmake.org/pipermail/cmake/2007-October/016924.html
#
#  Copyright (c) 2006-2011 Mathieu Malaterre <mathieu.malaterre@gmail.com>
#
#  Redistribution and use is allowed according to the terms of the New
#  BSD license.
#  For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#

macro(install_swig_module module_name module_type)
  # The following trick permits installation of module to the right destination:
  # binary path for dll (on windows)
  # library for non-dll platform
  if(WIN32)
    set(MODDST ${GDCM_INSTALL_BIN_DIR})
  else()
    set(MODDST ${GDCM_INSTALL_LIB_DIR})
  endif()
  string(TOUPPER ${module_type}Module MODTYPE)
  set(MODDIR GDCM_INSTALL_${MODTYPE}_DIR)
  # if user sets a GDCM_INSTALL_PYTHONMODULE_DIR
  if(${MODDIR})
    SET(MODDST "${${MODDIR}}")
  endif()
  if(NOT GDCM_INSTALL_NO_LIBRARIES)
    install(TARGETS ${SWIG_MODULE_${module_name}_REAL_NAME}
      RUNTIME DESTINATION ${MODDST} COMPONENT ${module_type}Module
      LIBRARY DESTINATION ${MODDST} COMPONENT ${module_type}Module
      )
  endif()
endmacro()

macro(install_library library)
  if(NOT GDCM_INSTALL_NO_LIBRARIES)
    # Runtime
    install(TARGETS ${library}
      EXPORT ${GDCM_TARGETS_NAME}
      RUNTIME DESTINATION ${GDCM_INSTALL_BIN_DIR} COMPONENT Applications
      LIBRARY DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT Libraries ${NAMELINK_SKIP}
      ARCHIVE DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT DebugDevel
      )
    # need recent cmake: http://cmake.org/gitweb?p=cmake.git;a=commitdiff;h=cbe7e8fa
    #export(EXPORT ${GDCM_TARGETS_NAME} APPEND FILE "${CMAKE_CURRENT_BINARY_DIR}/foo.cmake")
    #Development
    if(NAMELINK_ONLY)
      install(TARGETS ${library}
        EXPORT ${GDCM_TARGETS_NAME}
        LIBRARY DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT DebugDevel ${NAMELINK_ONLY}
        )
    endif()
  endif()
endmacro()

macro (install_pdb library)
  if(BUILD_SHARED_LIBS)
    if (MSVC AND FALSE) # Disabled for ITK
      install (
        FILES          "$<TARGET_PDB_FILE:${library}>"
        DESTINATION    "${GDCM_INSTALL_BIN_DIR}"
        COMPONENT      DebugDevel
        CONFIGURATIONS Debug RelWithDebInfo
      )
    endif ()
  endif ()
endmacro ()

# At least one argument is required
macro (install_includes glob_expression)
  if(NOT GDCM_INSTALL_NO_DEVELOPMENT)
    file(GLOB header_files ${glob_expression} ${ARGN})
    install(FILES ${header_files}
      DESTINATION ${GDCM_INSTALL_INCLUDE_DIR} COMPONENT Headers
      )
  endif()
endmacro ()
