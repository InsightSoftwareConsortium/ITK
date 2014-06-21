set(ITKIOGDCM_SRC
itkGDCMImageIO.cxx
itkGDCMImageIOFactory.cxx
itkGDCMSeriesFileNames.cxx
)

add_library(ITKIOGDCM ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOGDCM_SRC})
target_link_libraries(ITKIOGDCM  ${ITKGDCM_LIBRARIES} ${ITKIOImageBase_LIBRARIES} ${ITKImageIntensity_LIBRARIES})
itk_module_target(ITKIOGDCM)
