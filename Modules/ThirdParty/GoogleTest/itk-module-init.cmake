option(ITK_USE_SYSTEM_GOOGLETEST "Use an outside build or installed source of GoogleTest. Set GTEST_ROOT to specify location." ${ITK_USE_SYSTEM_LIBRARIES})
mark_as_advanced(ITK_USE_SYSTEM_GOOGLETEST)
if(ITK_USE_SYSTEM_GOOGLETEST)

  # The recommendation from google test is NOT to compile google tests
  # for system installs, but allow users of the library to include the
  # source in their project.

  set( GTEST_SRC_SEARCH_PATHS
    "/usr/src/googletest"
    "/usr/src/gtest"
    )

  if (NOT DEFINED GTEST_ROOT)
    foreach( search_path ${GTEST_SRC_SEARCH_PATHS})
      if(EXISTS "${search_path}/CMakeLists.txt")
        set( GTEST_ROOT "${search_path}")
        break()
      endif()
    endforeach()
  endif()

  if( DEFINED GTEST_ROOT AND EXISTS "${GTEST_ROOT}/CMakeLists.txt")

    find_path(GTEST_INCLUDE_DIRS gtest/gtest.h
      PATHS "${GTEST_ROOT}"
      NO_DEFAULT_PATH)

    if("${GTEST_INCLUDE_DIRS}" STREQUAL "")
      message(WARNING "GTEST_ROOT appears to be a source directory \
          but \"gtest/gtest.h\"  can not be found in source directory: \
          ${GTEST_ROOT}")
     endif()

   else()
     find_package( GTest REQUIRED )
   endif ()
endif()
