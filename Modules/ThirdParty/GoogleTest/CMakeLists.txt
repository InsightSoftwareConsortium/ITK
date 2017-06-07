project(ITKGoogleTest)
set(ITKGoogleTest_THIRD_PARTY 1)

if(ITK_USE_SYSTEM_GOOGLETEST)

  # The recommendation from google test is NOT to compile google tests
  # for system installs, but allow users of the library to include the
  # source in their project.
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
     set(ITKGoogleTest_NO_SRC 1)

     add_library(ITK::GTest ALIAS GTEST::GTest)
     add_library(ITK::GTestMain ALIAS GTEST::Main)
   endif ()
endif()

itk_module_impl()
