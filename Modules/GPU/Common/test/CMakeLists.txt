create_test_sourcelist(Tests ITKGPUCommon-tests.cxx
itkGPUImageTest.cxx
itkGPUImageFilterTest.cxx
)

add_executable(ITKGPUCommon-tests  ${Tests} )
itk_module_target_label(ITKGPUCommon-tests)
target_link_libraries(ITKGPUCommon-tests  ${ITKGPUCommon_LIBRARIES} )

itk_add_test(NAME itkGPUImageFilterTest
        COMMAND ITKGPUCommon-tests itkGPUImageFilterTest
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png
        ${TEMP}/gpuImageFilterTest.png)
