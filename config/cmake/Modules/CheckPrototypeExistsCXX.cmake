#
# This checks if a prototype for FUNC (with C linkage) has been
# declared in any one of the header files listed in INCLUDE. It uses
# the C++ compiler.
#
# (The check is actually whether declaring a prototype will cause a
# conflict and thus an error. The results may differ depending on the
# compiler. For example, gcc under Cygwin will issue a warning but g++
# will issue an error. In the DCMTK, the prototypes are used in a C++
# context, so we use the C++ compiler to check.
#
MACRO(CHECK_PROTOTYPE_EXISTS_CXX FUNC INCLUDE VARIABLE)
  IF("${VARIABLE}" MATCHES "^${VARIABLE}$")
    SET( CHECK_PROTOTYPE_EXISTS_CXX_FILE_IN "${VXL_CMAKE_DIR}/CheckPrototypeExists.cxx.in" )
    SET( CHECK_PROTOTYPE_EXISTS_CXX_FILE "${CMAKE_BINARY_DIR}/CMakeTmp/CheckPrototypeExists.cxx" )
    SET( CHECK_PROTOTYPE_EXISTS_CXX_EXTERNC_BEGIN "extern \"C\" {\n" )
    SET( CHECK_PROTOTYPE_EXISTS_CXX_EXTERNC_END "}\n" )

    SET(MACRO_CHECK_PROTOTYPE_EXISTS_CXX_FLAGS ${CMAKE_REQUIRED_FLAGS})
    MESSAGE(STATUS "Looking for prototype for ${FUNC} in ${INCLUDE}")

    SET( ${VARIABLE} 0 )
    FOREACH(FILE ${INCLUDE})

      # First check if the header exists. Cache the result in a variable named after
      # the header, so that we don't re-do the effort
      STRING( REGEX REPLACE "\\.|/" "_" CLEAN_FILE ${FILE} )
      SET( CHECK_PROTOTYPE_EXISTS_CXX_INCLUDE "CHECK_PROTOTYPE_EXISTS_CXX_INCLUDE_${CLEAN_FILE}" )
      CHECK_INCLUDE_FILE( ${FILE} ${CHECK_PROTOTYPE_EXISTS_CXX_INCLUDE} )
      IF( CHECK_PROTOTYPE_EXISTS_CXX_INCLUDE )

        FILE(APPEND ${CMAKE_BINARY_DIR}/CMakeOutput.log "Trying struct with ${FILE}\n" )
        CONFIGURE_FILE( ${CHECK_PROTOTYPE_EXISTS_CXX_FILE_IN}
                        ${CHECK_PROTOTYPE_EXISTS_CXX_FILE} IMMEDIATE )

        TRY_COMPILE( CHECK_PROTOTYPE_EXISTS_CXX_RESULT
          ${CMAKE_BINARY_DIR}
          ${CHECK_PROTOTYPE_EXISTS_CXX_FILE}
          CMAKE_FLAGS 
          -DCOMPILE_DEFINITIONS:STRING=${MACRO_CHECK_PROTOTYPE_EXISTS_CXX_FLAGS}
          OUTPUT_VARIABLE OUTPUT)
        IF( CHECK_PROTOTYPE_EXISTS_CXX_RESULT ) 
          FILE(APPEND ${CMAKE_BINARY_DIR}/CMakeOutput.log 
            "Determining if prototype ${FUNC} exists in ${FILE} "
            "failed with the following output:\n"
            "${OUTPUT}\n\n")
        ELSE( CHECK_PROTOTYPE_EXISTS_CXX_RESULT ) 
          FILE(APPEND ${CMAKE_BINARY_DIR}/CMakeOutput.log 
            "Determining if prototype ${FUNC} exists in ${FILE} "
            "passed with the following output:\n"
            "${OUTPUT}\n\n")
          MESSAGE(STATUS "    Found in ${FILE}")
          SET( ${VARIABLE} 1 )
        ENDIF( CHECK_PROTOTYPE_EXISTS_CXX_RESULT )

      ENDIF( CHECK_PROTOTYPE_EXISTS_CXX_INCLUDE )
    ENDFOREACH(FILE)

    IF( ${VARIABLE} )
      MESSAGE(STATUS "Looking for prototype of ${FUNC} - found")
      SET(${VARIABLE} 1 CACHE INTERNAL "Have prototype ${VARIABLE}")
    ELSE(${VARIABLE})
      MESSAGE(STATUS "Looking for prototype of ${FUNC} - not found")
      SET(${VARIABLE} "" CACHE INTERNAL "Have prototype ${VARIABLE}")
    ENDIF(${VARIABLE})
  ENDIF("${VARIABLE}" MATCHES "^${VARIABLE}$")
ENDMACRO(CHECK_PROTOTYPE_EXISTS_CXX)
