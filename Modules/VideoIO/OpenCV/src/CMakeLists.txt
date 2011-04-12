set(Video-IO-OpenCV_SRC
itkOpenCVVideoIO.cxx
itkOpenCVVideoIOFactory.cxx
)

add_library(Video-IO-OpenCV ${Video-IO-OpenCV_SRC})
target_link_libraries(Video-IO-OpenCV ${ITK-Common_LIBRARIES} ${ITK-IO-Base} Video-IO-Base ${TEST_DEPENDS_LIBRARIES} ${ITK-TestKernel_LIBRARIES})
itk_module_target(Video-IO-OpenCV)
