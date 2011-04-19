set(ITK-IO-LSM_SRC
itkLSMImageIOFactory.cxx
itkLSMImageIO.cxx
)

add_library(ITK-IO-LSM ${ITK-IO-LSM_SRC})
target_link_libraries(ITK-IO-LSM  ${ITK-IO-TIFF_LIBRARIES})
itk_module_target(ITK-IO-LSM)
