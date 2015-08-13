set(ITKIOGDCM_SRC
itkGDCMImageIO.cxx
itkGDCMImageIOFactory.cxx
itkGDCMSeriesFileNames.cxx
)

add_library(ITKIOGDCM ${ITKIOGDCM_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIOGDCM)
