set(ITKIONRRD_SRC
itkNrrdImageIOFactory.cxx
itkNrrdImageIO.cxx
)

add_library(ITKIONRRD ${ITK_LIBRARY_BUILD_TYPE} ${ITKIONRRD_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIONRRD)
