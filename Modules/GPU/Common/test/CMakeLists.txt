create_test_sourcelist(Tests ITK-GPUCommon-tests.cxx
itkGPUImageTest.cxx
itkGPUImageFilterTest.cxx
)

add_executable(ITK-GPUCommon-tests  ${Tests} )
target_link_libraries(ITK-GPUCommon-tests  ${ITK-GPUCommon_LIBRARIES} )

add_test(NAME itkGPUImageFilterTest
        COMMAND ITK-GPUCommon-tests itkGPUImageFilterTest
        ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png
        ${TEMP}/gpuImageFilterTest.png)
