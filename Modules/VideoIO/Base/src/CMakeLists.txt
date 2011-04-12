set(Video-IO-Base_SRC
itkVideoIOBase.cxx
itkVideoIOFactory.cxx
)

add_library(Video-IO-Base ${Video-IO-Base_SRC})
target_link_libraries(Video-IO-Base ${ITK-Common_LIBRARIES} ${ITK-IO-Base} ${TEST_DEPENDS_LIBRARIES} ${ITK-TestKernel_LIBRARIES})
itk_module_target(Video-IO-Base)
