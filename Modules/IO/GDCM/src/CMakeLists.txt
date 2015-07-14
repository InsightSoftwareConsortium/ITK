set(ITKIOGDCM_SRC
itkGDCMImageIO.cxx
itkGDCMImageIOFactory.cxx
itkGDCMSeriesFileNames.cxx
)

add_library(ITKIOGDCM ${ITKIOGDCM_SRC})
itk_module_link_dependencies()
target_link_libraries(ITKIOGDCM LINK_PUBLIC ${ITKImageIntensity_LIBRARIES})
itk_module_target(ITKIOGDCM)
