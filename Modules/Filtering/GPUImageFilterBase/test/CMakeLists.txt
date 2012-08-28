if (ITK_USE_GPU)

# With MS compilers on Win64, we need the /bigobj switch, else generated
# code results in objects with number of sections exceeding object file
# format.
# see http://msdn.microsoft.com/en-us/library/ms173499.aspx
if(CMAKE_CL_64 OR CMAKE_COMPILER_2005)
add_definitions(/bigobj)
endif()

itk_module_test()

set(ITKGPUImageFilterBase-tests
 itkGPUNeighborhoodOperatorImageFilterTest.cxx
)

CreateTestDriver(ITKGPUImageFilterBase "${ITKGPUImageFilterBase-Test_LIBRARIES}" "${ITKGPUImageFilterBase-tests}")

itk_add_test(NAME itkGPUNeighborhoodOperatorImageFilterTest2D
        COMMAND ITKGPUImageFilterBaseTestDriver itkGPUNeighborhoodOperatorImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_TEST_OUTPUT_DIR}/gpuNeighborhoodOperatorImageFilterTest2D.mha 2)

itk_add_test(NAME itkGPUNeighborhoodOperatorImageFilterTest3D
        COMMAND ITKGPUImageFilterBaseTestDriver itkGPUNeighborhoodOperatorImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_TEST_OUTPUT_DIR}/gpuNeighborhoodOperatorImageFilterTest3D.mha 3)

endif()
