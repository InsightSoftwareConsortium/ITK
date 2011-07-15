set(ITKBioCell_SRC
itkBioGenome.cxx
itkBioGene.cxx
itkBioGeneNetwork.cxx
itkBioCellBase.cxx
itkBioCellularAggregateBase.cxx
)

add_library(ITKBioCell ${ITKBioCell_SRC})
target_link_libraries(ITKBioCell  ${ITKMesh_LIBRARIES})
itk_module_target(ITKBioCell)
