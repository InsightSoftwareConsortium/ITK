set(ITKBioCell_SRC
itkBioGenome.cxx
itkBioGene.cxx
itkBioGeneNetwork.cxx
itkBioCellBase.cxx
itkBioCellularAggregateBase.cxx
)

add_library(ITKBioCell ${ITKBioCell_SRC})
itk_module_link_dependencies()
itk_module_target(ITKBioCell)
