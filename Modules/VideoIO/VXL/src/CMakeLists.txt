set(Video-IO-VXL_SRC
itkVXLVideoIO.cxx
itkVXLVideoIOFactory.cxx
)

add_library(Video-IO-VXL ${Video-IO-VXL_SRC})
target_link_libraries(Video-IO-VXL ${ITK-Common_LIBRARIES} ${ITK-IO-Base} Video-IO-Base ${TEST_DEPENDS_LIBRARIES} ${ITK-TestKernel_LIBRARIES})
itk_module_target(Video-IO-VXL)
