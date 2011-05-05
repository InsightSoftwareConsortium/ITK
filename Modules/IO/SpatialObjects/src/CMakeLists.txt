set(ITK-IO-SpatialObjects_SRC
itkPolygonGroupSpatialObjectXMLFile.cxx
)

add_library(ITK-IO-SpatialObjects ${ITK-IO-SpatialObjects_SRC})
target_link_libraries(ITK-IO-SpatialObjects  ${ITK-SpatialObjects_LIBRARIES} ${ITK-IO-XML_LIBRARIES} ${ITK-IO-Base_LIBRARIES})
itk_module_target(ITK-IO-SpatialObjects)
