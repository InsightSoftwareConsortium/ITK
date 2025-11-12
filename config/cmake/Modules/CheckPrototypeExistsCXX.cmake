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
macro(CHECK_PROTOTYPE_EXISTS_CXX FUNC INCLUDE VARIABLE)
  if(NOT DEFINED "${VARIABLE}")
    set( CHECK_PROTOTYPE_EXISTS_CXX_FILE_IN "${VXL_CMAKE_DIR}/CheckPrototypeExists.cxx.in" )
    set( CHECK_PROTOTYPE_EXISTS_CXX_FILE "${CMAKE_BINARY_DIR}/CMakeTmp/CheckPrototypeExists.cxx" )
    set( CHECK_PROTOTYPE_EXISTS_CXX_EXTERNC_BEGIN "extern \"C\" {\n" )
    set( CHECK_PROTOTYPE_EXISTS_CXX_EXTERNC_END "}\n" )

    set(MACRO_CHECK_PROTOTYPE_EXISTS_CXX_FLAGS ${CMAKE_REQUIRED_FLAGS})
    message(STATUS "Looking for prototype for ${FUNC} in ${INCLUDE}")

    set( ${VARIABLE} 0 )
    foreach(FILE ${INCLUDE})

      # First check if the header exists. Cache the result in a variable named after
      # the header, so that we don't re-do the effort
      string( REGEX REPLACE "\\.|/" "_" CLEAN_FILE ${FILE} )
      set( CHECK_PROTOTYPE_EXISTS_CXX_INCLUDE "CHECK_PROTOTYPE_EXISTS_CXX_INCLUDE_${CLEAN_FILE}" )
      CHECK_INCLUDE_FILE( ${FILE} ${CHECK_PROTOTYPE_EXISTS_CXX_INCLUDE} )
      if( CHECK_PROTOTYPE_EXISTS_CXX_INCLUDE )

        file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log "Trying struct with ${FILE}\n" )
        configure_file( ${CHECK_PROTOTYPE_EXISTS_CXX_FILE_IN}
                        ${CHECK_PROTOTYPE_EXISTS_CXX_FILE} @ONLY)

        try_compile( CHECK_PROTOTYPE_EXISTS_CXX_RESULT
          ${CMAKE_BINARY_DIR}
          ${CHECK_PROTOTYPE_EXISTS_CXX_FILE}
          CMAKE_FLAGS
          -DCOMPILE_DEFINITIONS:STRING=${MACRO_CHECK_PROTOTYPE_EXISTS_CXX_FLAGS}
          OUTPUT_VARIABLE OUTPUT)
        if( CHECK_PROTOTYPE_EXISTS_CXX_RESULT )
          file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
            "Determining if prototype ${FUNC} exists in ${FILE} "
            "failed with the following output:\n"
            "${OUTPUT}\n\n")
        else()
          file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
            "Determining if prototype ${FUNC} exists in ${FILE} "
            "passed with the following output:\n"
            "${OUTPUT}\n\n")
          message(STATUS "    Found in ${FILE}")
          set( ${VARIABLE} 1 )
        endif()

      endif()
    endforeach()

    if( ${VARIABLE} )
      message(STATUS "Looking for prototype of ${FUNC} - found")
      set(${VARIABLE} 1 CACHE INTERNAL "Have prototype ${VARIABLE}")
    else()
      message(STATUS "Looking for prototype of ${FUNC} - not found")
      set(${VARIABLE} "" CACHE INTERNAL "Have prototype ${VARIABLE}")
    endif()
  endif()
endmacro()
