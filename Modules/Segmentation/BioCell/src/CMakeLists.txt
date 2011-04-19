set(ITK-BioCell_SRC
itkBioGenome.cxx
itkBioGene.cxx
itkBioGeneNetwork.cxx
itkBioCellBase.cxx
itkBioCellularAggregateBase.cxx
)

add_library(ITK-BioCell ${ITK-BioCell_SRC})
target_link_libraries(ITK-BioCell  ${ITK-Mesh_LIBRARIES})
itk_module_target(ITK-BioCell)
