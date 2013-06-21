set(ITKIOTransformBase_SRC
itkTransformFactoryBase.cxx
)

add_library(ITKIOTransformBase ${ITKIOTransformBase_SRC})
target_link_libraries(ITKIOTransformBase ${ITKTransform_LIBRARIES})
itk_module_target(ITKIOTransformBase)
