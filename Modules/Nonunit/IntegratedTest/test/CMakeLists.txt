itk_module_test()
set(ITK-IntegratedTestTests
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

CreateTestDriver_SupportBuildInIOFactories(ITK-IntegratedTest  "${ITK-IntegratedTest-Test_LIBRARIES}" "${ITK-IntegratedTestTests}")

# temporary solution for all print tests. Each module should have its own print test later.
add_executable(itkAlgorithmsPrintTest  itkAlgorithmsPrintTest.cxx)
itk_module_target_label(itkAlgorithmsPrintTest)
target_link_libraries(itkAlgorithmsPrintTest ${ITK-IntegratedTest-Test_LIBRARIES})
itk_add_test(NAME itkAlgorithmsPrintTest COMMAND itkAlgorithmsPrintTest)

add_executable(itkAlgorithmsPrintTest2  itkAlgorithmsPrintTest2.cxx)
itk_module_target_label(itkAlgorithmsPrintTest2)
target_link_libraries(itkAlgorithmsPrintTest2 ${ITK-IntegratedTest-Test_LIBRARIES})
itk_add_test(NAME itkAlgorithmsPrintTest2 COMMAND itkAlgorithmsPrintTest)

add_executable(itkAlgorithmsPrintTest3  itkAlgorithmsPrintTest3.cxx)
itk_module_target_label(itkAlgorithmsPrintTest3)
target_link_libraries(itkAlgorithmsPrintTest3 ${ITK-IntegratedTest-Test_LIBRARIES})
itk_add_test(NAME itkAlgorithmsPrintTest3 COMMAND itkAlgorithmsPrintTest)

add_executable(itkAlgorithmsPrintTest4  itkAlgorithmsPrintTest4.cxx)
itk_module_target_label(itkAlgorithmsPrintTest4)
target_link_libraries(itkAlgorithmsPrintTest4 ${ITK-IntegratedTest-Test_LIBRARIES})
itk_add_test(NAME itkAlgorithmsPrintTest4 COMMAND itkAlgorithmsPrintTest)

#-------------------------------------------------------------------------
itk_add_test(NAME itkStatisticsPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkStatisticsPrintTest)
itk_add_test(NAME itkSpatialObjectPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkSpatialObjectPrintTest)
itk_add_test(NAME itkIOPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkIOPrintTest)
itk_add_test(NAME itkCommonPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkCommonPrintTest)
itk_add_test(NAME itkBasicFiltersPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkBasicFiltersPrintTest)
itk_add_test(NAME itkNeuralNetworksPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkNeuralNetworksPrintTest)
itk_add_test(NAME itkNumericsPrintTest
     COMMAND ITK-IntegratedTestTestDriver itkNumericsPrintTest)
