set(
  ITKIOTransformDCMTK_SRC
  itkDCMTKTransformIO.cxx
  itkDCMTKTransformIOFactory.cxx
)

add_library(ITKIOTransformDCMTK ${ITKIOTransformDCMTK_SRC})
target_link_libraries(
  ITKIOTransformDCMTK
  ${ITKIOTransformBase_LIBRARIES}
  ${ITKDCMTK_LIBRARIES}
)
itk_module_target(ITKIOTransformDCMTK)
