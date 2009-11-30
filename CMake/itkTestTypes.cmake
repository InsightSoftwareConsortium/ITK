# based on VTK/CMake/vtkTestTypes.cmake 11/13/09 -BCL

CHECK_TYPE_SIZE("long long" ITK_SIZEOF_LONG_LONG)
CHECK_TYPE_SIZE("__int64"   ITK_SIZEOF___INT64)

# The only compiler that satisfies the criteria for __int64 specified 
# in this file is Borland, however from some reason the NumericTraits
#  are not funcitoning therefore we had no systems which correctly 
# support __int64 as a fundemental type.
#
# Disable trying to even use __int64
SET( ITK_TRY_TO_USE__INT64 0 )

IF( ITK_TRY_TO_USE__INT64 )
  IF(HAVE_ITK_SIZEOF___INT64)
    # In CMake 2.6 and above the type __int64 may have been found only
    # due to inclusion of a system header.  Further try-compiles using
    # the type should include the header too.
    SET(_HAVE_DEFS)
    FOREACH(def HAVE_SYS_TYPES_H HAVE_STDINT_H HAVE_STDDEF_H)
      IF(${def})
        LIST(APPEND _HAVE_DEFS -D${def})
      ENDIF(${def})
    ENDFOREACH(def)

    IF("ITK_TYPE_SAME_LONG_AND___INT64" MATCHES "^ITK_TYPE_SAME_LONG_AND___INT64$")
      MESSAGE(STATUS "Checking whether long and __int64 are the same type")
      TRY_COMPILE(ITK_TYPE_SAME_LONG_AND___INT64
        ${ITK_BINARY_DIR}/CMakeTmp
        ${ITK_SOURCE_DIR}/CMake/itkTestCompareTypes.cxx
        COMPILE_DEFINITIONS
        -DITK_TEST_COMPARE_TYPE_1=long
        -DITK_TEST_COMPARE_TYPE_2=__int64
        ${_HAVE_DEFS}
        OUTPUT_VARIABLE OUTPUT)
      IF(ITK_TYPE_SAME_LONG_AND___INT64)
        MESSAGE(STATUS "Checking whether long and __int64 are the same type -- yes")
        SET(ITK_TYPE_SAME_LONG_AND___INT64 1 CACHE INTERNAL "Whether long and __int64 are the same type")
        WRITE_FILE(${CMAKE_BINARY_DIR}/CMakeFiles/CMakeOutput.log
          "Determining whether long and __int64 are the same type "
          "passed with the following output:\n"
          "${OUTPUT}\n" APPEND)
      ELSE(ITK_TYPE_SAME_LONG_AND___INT64)
        MESSAGE(STATUS "Checking whether long and __int64 are the same type -- no")
        SET(ITK_TYPE_SAME_LONG_AND___INT64 0 CACHE INTERNAL "Whether long and __int64 are the same type")
        WRITE_FILE(${CMAKE_BINARY_DIR}/CMakeFiles/CMakeError.log
          "Determining whether long and __int64 are the same type "
          "failed with the following output:\n"
          "${OUTPUT}\n" APPEND)
      ENDIF(ITK_TYPE_SAME_LONG_AND___INT64)
    ENDIF("ITK_TYPE_SAME_LONG_AND___INT64" MATCHES "^ITK_TYPE_SAME_LONG_AND___INT64$")

    IF(HAVE_ITK_SIZEOF_LONG_LONG)
      IF("ITK_TYPE_SAME_LONG_LONG_AND___INT64" MATCHES "^ITK_TYPE_SAME_LONG_LONG_AND___INT64$")
        MESSAGE(STATUS "Checking whether long long and __int64 are the same type")
        TRY_COMPILE(ITK_TYPE_SAME_LONG_LONG_AND___INT64
          ${ITK_BINARY_DIR}/CMakeTmp
          ${ITK_SOURCE_DIR}/CMake/itkTestCompareTypes.cxx
          COMPILE_DEFINITIONS
          -DITK_TEST_COMPARE_TYPE_1=TYPE_LONG_LONG
          -DITK_TEST_COMPARE_TYPE_2=__int64
          ${_HAVE_DEFS}
          OUTPUT_VARIABLE OUTPUT)
        IF(ITK_TYPE_SAME_LONG_LONG_AND___INT64)
          MESSAGE(STATUS "Checking whether long long and __int64 are the same type -- yes")
          SET(ITK_TYPE_SAME_LONG_LONG_AND___INT64 1 CACHE INTERNAL "Whether long long and __int64 are the same type")
          WRITE_FILE(${CMAKE_BINARY_DIR}/CMakeFiles/CMakeOutput.log
            "Determining whether long long and __int64 are the same type "
            "passed with the following output:\n"
            "${OUTPUT}\n" APPEND)
        ELSE(ITK_TYPE_SAME_LONG_LONG_AND___INT64)
          MESSAGE(STATUS "Checking whether long long and __int64 are the same type -- no")
          SET(ITK_TYPE_SAME_LONG_LONG_AND___INT64 0 CACHE INTERNAL "Whether long long and __int64 are the same type")
          WRITE_FILE(${CMAKE_BINARY_DIR}/CMakeFiles/CMakeError.log
            "Determining whether long long and __int64 are the same type "
            "failed with the following output:\n"
            "${OUTPUT}\n" APPEND)
        ENDIF(ITK_TYPE_SAME_LONG_LONG_AND___INT64)
      ENDIF("ITK_TYPE_SAME_LONG_LONG_AND___INT64" MATCHES "^ITK_TYPE_SAME_LONG_LONG_AND___INT64$")
    ENDIF(HAVE_ITK_SIZEOF_LONG_LONG)
    IF(NOT ITK_TYPE_SAME_LONG_AND___INT64)
      IF(NOT ITK_TYPE_SAME_LONG_LONG_AND___INT64)
        #  VS 6 cannot convert unsigned __int64 to double unless the
        # "Visual C++ Processor Pack" is installed.
        IF("ITK_TYPE_CONVERT_UI64_TO_DOUBLE" MATCHES "^ITK_TYPE_CONVERT_UI64_TO_DOUBLE$")
          MESSAGE(STATUS "Checking whether unsigned __int64 can convert to double")
          TRY_COMPILE(ITK_TYPE_CONVERT_UI64_TO_DOUBLE
            ${ITK_BINARY_DIR}/CMakeTmp
            ${ITK_SOURCE_DIR}/CMake/itkTestConvertTypes.cxx
            COMPILE_DEFINITIONS
            -DITK_TEST_CONVERT_TYPE_FROM=TYPE_UNSIGNED___INT64
            -DITK_TEST_CONVERT_TYPE_TO=double
            ${_HAVE_DEFS}
            OUTPUT_VARIABLE OUTPUT)
          IF(ITK_TYPE_CONVERT_UI64_TO_DOUBLE)
            MESSAGE(STATUS "Checking whether unsigned __int64 can convert to double -- yes")
            SET(ITK_TYPE_CONVERT_UI64_TO_DOUBLE 1 CACHE INTERNAL "Whether unsigned __int64 can convert to double")
            WRITE_FILE(${CMAKE_BINARY_DIR}/CMakeFiles/CMakeOutput.log
              "Determining whether unsigned __int64 can convert to double "
              "passed with the following output:\n"
              "${OUTPUT}\n" APPEND)
          ELSE(ITK_TYPE_CONVERT_UI64_TO_DOUBLE)
            MESSAGE(STATUS "Checking whether unsigned __int64 can convert to double -- no")
            SET(ITK_TYPE_CONVERT_UI64_TO_DOUBLE 0 CACHE INTERNAL "Whether unsigned __int64 can convert to double")
            WRITE_FILE(${CMAKE_BINARY_DIR}/CMakeFiles/CMakeError.log
              "Determining whether unsigned __int64 can convert to double "
              "failed with the following output:\n"
              "${OUTPUT}\n" APPEND)
          ENDIF(ITK_TYPE_CONVERT_UI64_TO_DOUBLE)
        ENDIF("ITK_TYPE_CONVERT_UI64_TO_DOUBLE" MATCHES "^ITK_TYPE_CONVERT_UI64_TO_DOUBLE$")
      ENDIF(NOT ITK_TYPE_SAME_LONG_LONG_AND___INT64)
    ENDIF(NOT ITK_TYPE_SAME_LONG_AND___INT64)
  ENDIF(HAVE_ITK_SIZEOF___INT64)
ENDIF ( ITK_TRY_TO_USE__INT64 )

# Enable the "long long" type if it is available.  It is standard in
# C99 and C++03 but not in earlier standards.
SET(ITK_TYPE_USE_LONG_LONG)
IF(HAVE_ITK_SIZEOF_LONG_LONG)
  SET(ITK_TYPE_USE_LONG_LONG 1)
ENDIF(HAVE_ITK_SIZEOF_LONG_LONG)

# Enable the "__int64" type if it is available and is a fundemental type.  It is not
# standard. It also must be convertable to double.
SET(ITK_TYPE_USE___INT64)

IF ( ITK_TRY_TO_USE__INT64 )
IF(HAVE_ITK_SIZEOF___INT64)
  IF(NOT ITK_TYPE_SAME_LONG_AND___INT64)
    IF(NOT ITK_TYPE_SAME_LONG_LONG_AND___INT64)
      IF(ITK_TYPE_CONVERT_UI64_TO_DOUBLE)       
        SET(ITK_TYPE_USE___INT64 1)
      ENDIF(ITK_TYPE_CONVERT_UI64_TO_DOUBLE)
    ENDIF(NOT ITK_TYPE_SAME_LONG_LONG_AND___INT64)
  ENDIF(NOT ITK_TYPE_SAME_LONG_AND___INT64)
ENDIF(HAVE_ITK_SIZEOF___INT64)
ENDIF( ITK_TRY_TO_USE__INT64 )
