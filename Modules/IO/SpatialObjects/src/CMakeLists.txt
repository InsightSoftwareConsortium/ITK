set(ITKIOSpatialObjects_SRC
itkPolygonGroupSpatialObjectXMLFile.cxx
)

add_library(ITKIOSpatialObjects ${ITKIOSpatialObjects_SRC})
target_link_libraries(ITKIOSpatialObjects  ${ITKSpatialObjects_LIBRARIES} ${ITKIOXML_LIBRARIES} ${ITKIOImageBase_LIBRARIES})
itk_module_target(ITKIOSpatialObjects)
