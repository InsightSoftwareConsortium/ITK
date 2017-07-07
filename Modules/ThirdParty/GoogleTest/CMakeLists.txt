project(ITKGoogleTest)
set(ITKGoogleTest_THIRD_PARTY 1)

if(ITK_USE_SYSTEM_GOOGLETEST)
  if( NOT DEFINED GTEST_ROOT OR NOT EXISTS "${GTEST_ROOT}/CMakeLists.txt")
    set(ITKGoogleTest_NO_SRC 1)
    set(ITKGoogleTest_LIBRARIES GTest::GTest GTest::Main)
    set(ITKGoogleTest_EXPORT_CODE_BUILD "
if(NOT ITK_BINARY_DIR)
  set(GTEST_ROOT \"${GTEST_ROOT}\")
  find_package(GTest REQUIRED)
endif()
")
  endif()
endif ()

itk_module_impl()
