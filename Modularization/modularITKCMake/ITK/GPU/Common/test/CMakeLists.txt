create_test_sourcelist(Tests ITK-GPUCommon-tests.cxx
test_itkGPUImage.cxx
test_itkGPUImageFilter.cxx
)

add_executable(ITK-GPUCommon-tests  ${Tests} )
target_link_libraries(ITK-GPUCommon-tests  ${ITK-GPUCommon_LIBRARIES} )

add_test(NAME test_itkGPUImageFilter
        COMMAND ITK-GPUCommon-tests test_itkGPUImageFilter
        ${ITK_DATA_ROOT}/../../Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/gpuImageFilterTest.png)
