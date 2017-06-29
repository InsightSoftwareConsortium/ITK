project(ITKGoogleTest)
set(ITKGoogleTest_THIRD_PARTY 1)

if(ITK_USE_SYSTEM_GOOGLETEST)
  if( NOT DEFINED GTEST_ROOT OR NOT EXISTS "${GTEST_ROOT}/CMakeLists.txt")
     set(ITKGoogleTest_NO_SRC 1)
   endif ()
endif()

itk_module_impl()
