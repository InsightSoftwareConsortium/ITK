set(ITK-SpatialObjects_SRC
itkCylinderSpatialObject.cxx
itkSpatialObjectFactoryBase.cxx
)

add_library(ITK-SpatialObjects ${ITK-SpatialObjects_SRC})
target_link_libraries(ITK-SpatialObjects  ${ITK-ImageFunction_LIBRARIES} ${ITK-Mesh_LIBRARIES} ${ITK-IO-Base_LIBRARIES})
itk_module_target(ITK-SpatialObjects)
