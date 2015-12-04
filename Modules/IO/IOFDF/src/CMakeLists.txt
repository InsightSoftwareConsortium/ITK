set(
  IOFDF_SRC
  itkFDFImageIO.cxx
  itkFDFCommonImageIO.cxx
  itkFDFImageIOFactory.cxx
)

add_library(
  IOFDF
  ${ITK_LIBRARY_BUILD_TYPE}
  ${IOFDF_SRC}
)

target_link_libraries(
  IOFDF
  ${ITKIOImageBase_LIBRARIES}
  ${ITKTransform_LIBRARIES}
)
itk_module_target(IOFDF)
