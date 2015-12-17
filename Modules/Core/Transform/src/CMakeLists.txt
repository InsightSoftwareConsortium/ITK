set(ITKTransform_SRC
itkTransformBase.cxx
)

add_library(ITKTransform ${ITK_LIBRARY_BUILD_TYPE} ${ITKTransform_SRC})
itk_module_link_dependencies()
itk_module_target(ITKTransform)
