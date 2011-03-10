set(ITK-IO-Spatialobjects_SRC
itkPolygonGroupSpatialObjectXMLFile.cxx
)

add_library(ITK-IO-Spatialobjects ${ITK-IO-Spatialobjects_SRC})
target_link_libraries(ITK-IO-Spatialobjects  ${ITK-SpatialObjects_LIBRARIES} ${ITK-IO-XML_LIBRARIES} ${ITK-IO-Base_LIBRARIES} ${TEST_DEPENDS_LIBRARIES} ${ITK-TestKernel_LIBRARIES})
itk_module_target(ITK-IO-Spatialobjects)
