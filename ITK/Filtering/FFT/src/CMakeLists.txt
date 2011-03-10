set(ITK-FFT_SRC
itkFFTWGlobalConfiguration.cxx
)

add_library(ITK-FFT ${ITK-FFT_SRC})
target_link_libraries(ITK-FFT  ${ITK-Common_LIBRARIES} ${TEST_DEPENDS_LIBRARIES} ${ITK-TestKernel_LIBRARIES})
itk_module_target(ITK-FFT)
