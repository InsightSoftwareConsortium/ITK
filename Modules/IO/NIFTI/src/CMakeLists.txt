set(ITKIONIFTI_SRC
itkNiftiImageIOFactory.cxx
itkNiftiImageIO.cxx
)

add_library(ITKIONIFTI ${ITK_LIBRARY_BUILD_TYPE} ${ITKIONIFTI_SRC})
itk_module_link_dependencies()
itk_module_target(ITKIONIFTI)
