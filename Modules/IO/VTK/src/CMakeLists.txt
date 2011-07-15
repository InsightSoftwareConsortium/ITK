set(ITKIOVTK_SRC
itkVTKImageIOFactory.cxx
itkVTKImageIO.cxx
)

add_library(ITKIOVTK ${ITKIOVTK_SRC})
target_link_libraries(ITKIOVTK  ${ITKIOBase_LIBRARIES})
itk_module_target(ITKIOVTK)
