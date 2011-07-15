itk_module_test()
set(ITKIntegratedTestTests
itkFilterImageAddTest.cxx # missing add_test() call
itkNumericsPrintTest.cxx
itkReleaseDataFilterTest.cxx # missing add_test() call
itkStatisticsPrintTest.cxx
itkCommonPrintTest.cxx
itkIOPrintTest.cxx
itkBasicFiltersPrintTest.cxx
itkNeuralNetworksPrintTest.cxx
itkSpatialObjectPrintTest.cxx
)

CreateTestDriver_SupportBuildInIOFactories(ITKIntegratedTest  "${ITKIntegratedTest-Test_LIBRARIES}" "${ITKIntegratedTestTests}")

# temporary solution for all print tests. Each module should have its own print test later.
add_executable(itkAlgorithmsPrintTest  itkAlgorithmsPrintTest.cxx)
itk_module_target_label(itkAlgorithmsPrintTest)
target_link_libraries(itkAlgorithmsPrintTest ${ITKIntegratedTest-Test_LIBRARIES})
itk_add_test(NAME itkAlgorithmsPrintTest COMMAND itkAlgorithmsPrintTest)

add_executable(itkAlgorithmsPrintTest2  itkAlgorithmsPrintTest2.cxx)
itk_module_target_label(itkAlgorithmsPrintTest2)
target_link_libraries(itkAlgorithmsPrintTest2 ${ITKIntegratedTest-Test_LIBRARIES})
itk_add_test(NAME itkAlgorithmsPrintTest2 COMMAND itkAlgorithmsPrintTest)

add_executable(itkAlgorithmsPrintTest3  itkAlgorithmsPrintTest3.cxx)
itk_module_target_label(itkAlgorithmsPrintTest3)
target_link_libraries(itkAlgorithmsPrintTest3 ${ITKIntegratedTest-Test_LIBRARIES})
itk_add_test(NAME itkAlgorithmsPrintTest3 COMMAND itkAlgorithmsPrintTest)

add_executable(itkAlgorithmsPrintTest4  itkAlgorithmsPrintTest4.cxx)
itk_module_target_label(itkAlgorithmsPrintTest4)
target_link_libraries(itkAlgorithmsPrintTest4 ${ITKIntegratedTest-Test_LIBRARIES})
itk_add_test(NAME itkAlgorithmsPrintTest4 COMMAND itkAlgorithmsPrintTest)

#-------------------------------------------------------------------------
itk_add_test(NAME itkStatisticsPrintTest
      COMMAND ITKIntegratedTestTestDriver itkStatisticsPrintTest)
itk_add_test(NAME itkSpatialObjectPrintTest
      COMMAND ITKIntegratedTestTestDriver itkSpatialObjectPrintTest)
itk_add_test(NAME itkIOPrintTest
      COMMAND ITKIntegratedTestTestDriver itkIOPrintTest)
itk_add_test(NAME itkCommonPrintTest
      COMMAND ITKIntegratedTestTestDriver itkCommonPrintTest)
itk_add_test(NAME itkBasicFiltersPrintTest
      COMMAND ITKIntegratedTestTestDriver itkBasicFiltersPrintTest)
itk_add_test(NAME itkNeuralNetworksPrintTest
      COMMAND ITKIntegratedTestTestDriver itkNeuralNetworksPrintTest)
itk_add_test(NAME itkNumericsPrintTest
     COMMAND ITKIntegratedTestTestDriver itkNumericsPrintTest)
