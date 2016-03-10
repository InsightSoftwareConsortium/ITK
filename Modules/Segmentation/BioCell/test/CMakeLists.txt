itk_module_test()
set(ITKBioCellTests
itkBioCellTest.cxx
itkBioGenomeTest.cxx
itkBioCellularAggregateTest.cxx
itkBioGeneNetworkTest.cxx
itkBioGeneTest.cxx
)

CreateTestDriver(ITKBioCell  "${ITKBioCell-Test_LIBRARIES}" "${ITKBioCellTests}")

itk_add_test(NAME itkBioCellTest
      COMMAND ITKBioCellTestDriver itkBioCellTest 1.0 1.0 0.1 2.0 1.0 1.0 10 10 30 150 255 220)
itk_add_test(NAME itkBioGenomeTest
      COMMAND ITKBioCellTestDriver itkBioGenomeTest)
itk_add_test(NAME itkBioCellularAggregateTest
      COMMAND ITKBioCellTestDriver itkBioCellularAggregateTest)
itk_add_test(NAME itkBioGeneNetworkTest
      COMMAND ITKBioCellTestDriver itkBioGeneNetworkTest)
itk_add_test(NAME itkBioGeneTest
      COMMAND ITKBioCellTestDriver itkBioGeneTest)
