# - C# module for CMake
# Defines the following macros:
#   CSHARP_ADD_EXECUTABLE(name [ files ])
#     - Define C# executable with given name
#   CSHARP_ADD_LIBRARY(name [ files ])
#     - Define C# library with given name
#   CSHARP_LINK_LIBRARIES(name [ libraries ])
#     - Link libraries to csharp library
#
#  Copyright (c) 2006-2010 Mathieu Malaterre <mathieu.malaterre@gmail.com>
#
#  Redistribution and use is allowed according to the terms of the New
#  BSD license.
#  For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#

# TODO:
# http://www.cs.nuim.ie/~jpower/Research/csharp/Index.html

IF(WIN32)
  INCLUDE(${DotNETFrameworkSDK_USE_FILE})
  # remap
  SET(CMAKE_CSHARP1_COMPILER ${CSC_v1_EXECUTABLE})
  SET(CMAKE_CSHARP2_COMPILER ${CSC_v2_EXECUTABLE})
  SET(CMAKE_CSHARP3_COMPILER ${CSC_v3_EXECUTABLE})

  #SET(CMAKE_CSHARP3_INTERPRETER ${MONO_EXECUTABLE})
ELSE(WIN32)
  INCLUDE(${MONO_USE_FILE})
  SET(CMAKE_CSHARP1_COMPILER ${MCS_EXECUTABLE})
  SET(CMAKE_CSHARP2_COMPILER ${GMCS_EXECUTABLE})
  SET(CMAKE_CSHARP3_COMPILER ${SMCS_EXECUTABLE})

  SET(CMAKE_CSHARP_INTERPRETER ${MONO_EXECUTABLE})
ENDIF(WIN32)

SET(DESIRED_CSHARP_COMPILER_VERSION 2 CACHE STRING "Pick a version for C# compiler to use: 1, 2 or 3")
MARK_AS_ADVANCED(DESIRED_CSHARP_COMPILER_VERSION)

# default to v1:
IF(DESIRED_CSHARP_COMPILER_VERSION MATCHES 1)
  SET(CMAKE_CSHARP_COMPILER ${CMAKE_CSHARP1_COMPILER})
ELSEIF(DESIRED_CSHARP_COMPILER_VERSION MATCHES 2)
  SET(CMAKE_CSHARP_COMPILER ${CMAKE_CSHARP2_COMPILER})
ELSEIF(DESIRED_CSHARP_COMPILER_VERSION MATCHES 3)
  SET(CMAKE_CSHARP_COMPILER ${CMAKE_CSHARP3_COMPILER})
ELSE(DESIRED_CSHARP_COMPILER_VERSION MATCHES 3)
  MESSAGE(FATAL_ERROR "Do not know this version")
ENDIF(DESIRED_CSHARP_COMPILER_VERSION MATCHES 1)

# Check something is found:
IF(NOT CMAKE_CSHARP_COMPILER)
  # status message only for now:
  MESSAGE("Sorry C# v${DESIRED_CSHARP_COMPILER_VERSION} was not found on your system")
ELSE(NOT CMAKE_CSHARP_COMPILER)
  #IF (NOT CSHARP_FIND_QUIETLY)
  MESSAGE("Will be using C# v${DESIRED_CSHARP_COMPILER_VERSION}: ${CMAKE_CSHARP_COMPILER}")
  #ENDIF (NOT CSHARP_FIND_QUIETLY)
ENDIF(NOT CMAKE_CSHARP_COMPILER)

MACRO(CSHARP_ADD_LIBRARY name)
  SET(csharp_cs_sources)
  SET(csharp_cs_sources_dep)
  FOREACH(it ${ARGN})
    IF(EXISTS ${it})
      SET(csharp_cs_sources "${csharp_cs_sources} ${it}")
      SET(csharp_cs_sources_dep ${csharp_cs_sources_dep} ${it})
    ELSE(EXISTS ${it})
      IF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${it})
        SET(csharp_cs_sources "${csharp_cs_sources} ${CMAKE_CURRENT_SOURCE_DIR}/${it}")
        SET(csharp_cs_sources_dep ${csharp_cs_sources_dep} ${CMAKE_CURRENT_SOURCE_DIR}/${it})
      ELSE(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${it})
        #MESSAGE("Could not find: ${it}")
        SET(csharp_cs_sources "${csharp_cs_sources} ${it}")
      ENDIF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${it})
    ENDIF(EXISTS ${it})
  ENDFOREACH(it)

  #SET(SHARP #)
  SEPARATE_ARGUMENTS(csharp_cs_sources)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${name}.dll
    COMMAND ${CMAKE_CSHARP_COMPILER}
    ARGS "/t:library" "/out:${name}.dll" ${csharp_cs_sources}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS "${csharp_cs_sources_dep}"
    COMMENT "Creating Csharp library ${name}.cs"
  )
  ADD_CUSTOM_TARGET(CSharp_${name} ALL
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${name}.dll
  )
ENDMACRO(CSHARP_ADD_LIBRARY)

MACRO(CSHARP_ADD_EXECUTABLE name)
  SET(csharp_cs_sources)
  FOREACH(it ${ARGN})
    IF(EXISTS ${it})
      SET(csharp_cs_sources "${csharp_cs_sources} ${it}")
    ELSE(EXISTS ${it})
      IF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${it})
        SET(csharp_cs_sources "${csharp_cs_sources} ${CMAKE_CURRENT_SOURCE_DIR}/${it}")
      ELSE(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${it})
        #MESSAGE("Could not find: ${it}")
        SET(csharp_cs_sources "${csharp_cs_sources} ${it}")
      ENDIF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${it})
    ENDIF(EXISTS ${it})
  ENDFOREACH(it)

  SET(CSHARP_EXECUTABLE_${name}_ARGS
    #"/out:${name}.dll" ${csharp_cs_sources}
    #"/r:gdcm_csharp.dll"
    "/out:${name}.exe ${csharp_cs_sources}"
  )

ENDMACRO(CSHARP_ADD_EXECUTABLE)

MACRO(CSHARP_LINK_LIBRARIES name)
  SET(csharp_libraries)
  SET(csharp_libraries_depends)
  FOREACH(it ${ARGN})
    #IF(EXISTS ${it}.dll)
      SET(csharp_libraries "${csharp_libraries} /r:${it}.dll")
    #  SET(csharp_libraries_depends ${it}.dll)
    #ELSE(EXISTS ${it}.dll)
    #  IF(EXISTS ${CMAKE_CURRENT_BINARY_DIR}/${it}.dll)
    #    SET(csharp_libraries "${csharp_libraries} /r:${it}.dll")
    #    SET(csharp_libraries_depends ${CMAKE_CURRENT_BINARY_DIR}/${it}.dll)
    #  ELSE(EXISTS ${CMAKE_CURRENT_BINARY_DIR}/${it}.dll)
    #    MESSAGE("Could not find: ${it}")
    #  ENDIF(EXISTS ${CMAKE_CURRENT_BINARY_DIR}/${it}.dll)
    #ENDIF(EXISTS ${it}.dll)
  ENDFOREACH(it)
  SET(CSHARP_EXECUTABLE_${name}_ARGS " ${csharp_libraries} ${CSHARP_EXECUTABLE_${name}_ARGS}")
  #MESSAGE( "DEBUG: ${CSHARP_EXECUTABLE_${name}_ARGS}" )

  # BAD DESIGN !
  # This should be in the _ADD_EXECUTABLE...
  SEPARATE_ARGUMENTS(CSHARP_EXECUTABLE_${name}_ARGS)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${name}.exe
    COMMAND ${CMAKE_CSHARP_COMPILER}
    #ARGS "/r:gdcm_csharp.dll" "/out:${name}.exe" ${csharp_cs_sources}
    ARGS ${CSHARP_EXECUTABLE_${name}_ARGS}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    #DEPENDS ${csharp_cs_sources}
    COMMENT "Create HelloWorld.exe"
  )

  #MESSAGE("DEBUG2:${csharp_libraries_depends}")
  ADD_CUSTOM_TARGET(CSHARP_EXECUTABLE_${name} ALL
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${name}.exe
            ${csharp_libraries_depends}
  )

ENDMACRO(CSHARP_LINK_LIBRARIES)
