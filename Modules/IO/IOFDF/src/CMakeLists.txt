set(
  ITKIOFDF_SRC
  itkFDFImageIO.cxx
  itkFDFCommonImageIO.cxx
  itkFDFImageIOFactory.cxx
)

add_library(ITKIOFDF ${ITKIOFDF_SRC})

target_link_libraries(
  ITKIOFDF
  ${ITKIOImageBase_LIBRARIES}
  ${ITKTransform_LIBRARIES}
)
itk_module_target(ITKIOFDF)
