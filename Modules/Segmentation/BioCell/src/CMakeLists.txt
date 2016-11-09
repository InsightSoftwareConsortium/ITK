set(ITKBioCell_SRCS
  itkBioGenome.cxx
  itkBioGene.cxx
  itkBioGeneNetwork.cxx
  itkBioCellBase.cxx
  itkBioCellularAggregateBase.cxx
  )

itk_module_add_library(ITKBioCell ${ITKBioCell_SRCS})
