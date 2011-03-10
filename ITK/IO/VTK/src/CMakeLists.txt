set(ITK-IO-VTK_SRC
itkVTKImageIOFactory.cxx
itkVTKImageIO.cxx
)

add_library(ITK-IO-VTK ${ITK-IO-VTK_SRC})
target_link_libraries(ITK-IO-VTK  ${ITK-IO-Base_LIBRARIES} ${TEST_DEPENDS_LIBRARIES} ${ITK-TestKernel_LIBRARIES})
itk_module_target(ITK-IO-VTK)
