set(ITKIOVTK_SRC
itkVTKImageIOFactory.cxx
itkVTKImageIO.cxx
)

add_library(ITKIOVTK ${ITK_LIBRARY_BUILD_TYPE} ${ITKIOVTK_SRC})
target_link_libraries(ITKIOVTK  ${ITKIOImageBase_LIBRARIES})
itk_module_target(ITKIOVTK)
