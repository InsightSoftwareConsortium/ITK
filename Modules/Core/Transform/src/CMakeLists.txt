set(ITKTransform_SRC
itkTransformFactoryBase.cxx
)

add_library(ITKTransform ${ITKTransform_SRC})
target_link_libraries(ITKTransform ${ITKCommon_LIBRARIES})
itk_module_target(ITKTransform)
