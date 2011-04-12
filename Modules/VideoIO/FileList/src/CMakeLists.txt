set(Video-IO-FileList_SRC
itkFileListVideoIO.cxx
itkFileListVideoIOFactory.cxx
)

add_library(Video-IO-FileList ${Video-IO-FileList_SRC})
target_link_libraries(Video-IO-FileList ${ITK-Common_LIBRARIES} ${ITK-IO-Base} Video-IO-Base ${TEST_DEPENDS_LIBRARIES} ${ITK-TestKernel_LIBRARIES})
itk_module_target(Video-IO-FileList)
