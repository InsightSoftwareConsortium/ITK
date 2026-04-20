itk_module_test()

set(StructuralSimilarityGTests itkStructuralSimilarityImageFilterGTest.cxx)

creategoogletestdriver(StructuralSimilarity "${StructuralSimilarity-Test_LIBRARIES}" "${StructuralSimilarityGTests}")
