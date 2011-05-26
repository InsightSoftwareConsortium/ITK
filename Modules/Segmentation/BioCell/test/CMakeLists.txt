itk_module_test()
set(ITK-BioCellTests
itkBioCellTest.cxx
itkBioGenomeTest.cxx
itkBioCellularAggregateTest.cxx
itkBioGeneNetworkTest.cxx
itkBioCellHeaderTest.cxx
itkBioGeneTest.cxx
)

CreateTestDriver(ITK-BioCell  "${ITK-BioCell-Test_LIBRARIES}" "${ITK-BioCellTests}")

add_test(NAME itkBioCellHeaderTest
      COMMAND ITK-BioCellTestDriver itkBioCellHeaderTest)
add_test(NAME itkBioCellTest
      COMMAND ITK-BioCellTestDriver itkBioCellTest)
add_test(NAME itkBioGenomeTest
      COMMAND ITK-BioCellTestDriver itkBioGenomeTest)
add_test(NAME itkBioCellularAggregateTest
      COMMAND ITK-BioCellTestDriver itkBioCellularAggregateTest)
add_test(NAME itkBioGeneNetworkTest
      COMMAND ITK-BioCellTestDriver itkBioGeneNetworkTest)
add_test(NAME itkBioGeneTest
      COMMAND ITK-BioCellTestDriver itkBioGeneTest)
