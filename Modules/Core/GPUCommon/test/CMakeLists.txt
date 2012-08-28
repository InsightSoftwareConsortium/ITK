if (ITK_USE_GPU)

# With MS compilers on Win64, we need the /bigobj switch, else generated
# code results in objects with number of sections exceeding object file
# format.
# see http://msdn.microsoft.com/en-us/library/ms173499.aspx
if(CMAKE_CL_64 OR CMAKE_COMPILER_2005)
add_definitions(/bigobj)
endif()

itk_module_test()

set(ITKGPUCommon-tests
 itkGPUImageTest.cxx
 itkGPUReductionTest.cxx
 )

CreateTestDriver(ITKGPUCommon "${ITKGPUCommon-Test_LIBRARIES}" "${ITKGPUCommon-tests}")
itk_add_test(NAME itkGPUImageTest COMMAND ITKGPUCommonTestDriver itkGPUImageTest)
itk_add_test(NAME itkGPUReductionTest COMMAND ITKGPUCommonTestDriver itkGPUReductionTest)

endif()
