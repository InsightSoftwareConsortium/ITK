itk_module_test()
set(ITKIntegratedTestTests
itkFilterImageAddTest.cxx # missing itk_add_test() call
itkNumericsPrintTest.cxx
itkReleaseDataFilterTest.cxx # missing itk_add_test() call
itkStatisticsPrintTest.cxx
itkCommonPrintTest.cxx
itkIOPrintTest.cxx
itkBasicFiltersPrintTest.cxx
itkNeuralNetworksPrintTest.cxx
itkSpatialObjectPrintTest.cxx
itkShrinkImagePreserveObjectPhysicalLocations.cxx
)

CreateTestDriver_SupportBuildInIOFactories(ITKIntegratedTest  "${ITKIntegratedTest-Test_LIBRARIES}" "${ITKIntegratedTestTests}")

set(TEMP ${ITK_TEST_OUTPUT_DIR})

# temporary solution for all print tests. Each module should have its own print test later.
add_executable(itkAlgorithmsPrintTest  itkAlgorithmsPrintTest.cxx)
itk_module_target_label(itkAlgorithmsPrintTest)
target_link_libraries(itkAlgorithmsPrintTest LINK_PUBLIC ${ITKIntegratedTest-Test_LIBRARIES})
itk_add_test(NAME itkAlgorithmsPrintTest COMMAND itkAlgorithmsPrintTest)

add_executable(itkAlgorithmsPrintTest2  itkAlgorithmsPrintTest2.cxx)
itk_module_target_label(itkAlgorithmsPrintTest2)
target_link_libraries(itkAlgorithmsPrintTest2 LINK_PUBLIC ${ITKIntegratedTest-Test_LIBRARIES})
itk_add_test(NAME itkAlgorithmsPrintTest2 COMMAND itkAlgorithmsPrintTest)

add_executable(itkAlgorithmsPrintTest3  itkAlgorithmsPrintTest3.cxx)
itk_module_target_label(itkAlgorithmsPrintTest3)
target_link_libraries(itkAlgorithmsPrintTest3 LINK_PUBLIC ${ITKIntegratedTest-Test_LIBRARIES})
itk_add_test(NAME itkAlgorithmsPrintTest3 COMMAND itkAlgorithmsPrintTest)

add_executable(itkAlgorithmsPrintTest4  itkAlgorithmsPrintTest4.cxx)
itk_module_target_label(itkAlgorithmsPrintTest4)
target_link_libraries(itkAlgorithmsPrintTest4 LINK_PUBLIC ${ITKIntegratedTest-Test_LIBRARIES})
itk_add_test(NAME itkAlgorithmsPrintTest4 COMMAND itkAlgorithmsPrintTest)

#-------------------------------------------------------------------------
itk_add_test(NAME itkStatisticsPrintTest
      COMMAND ITKIntegratedTestTestDriver itkStatisticsPrintTest)
itk_add_test(NAME itkSpatialObjectPrintTest
      COMMAND ITKIntegratedTestTestDriver itkSpatialObjectPrintTest)
itk_add_test(NAME itkIOPrintTest
      COMMAND ITKIntegratedTestTestDriver itkIOPrintTest)

itk_add_test(NAME itkCommonPrintTest
      COMMAND ITKIntegratedTestTestDriver  --redirectOutput ${TEMP}/itkCommonPrintTest.txt
    itkCommonPrintTest)
set_tests_properties(itkCommonPrintTest PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkCommonPrintTest.txt)

itk_add_test(NAME itkBasicFiltersPrintTest
      COMMAND ITKIntegratedTestTestDriver   --redirectOutput ${TEMP}/itkBasicFiltersPrintTest.txt
    itkBasicFiltersPrintTest)
set_tests_properties(itkBasicFiltersPrintTest PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkBasicFiltersPrintTest.txt)

itk_add_test(NAME itkNeuralNetworksPrintTest
      COMMAND ITKIntegratedTestTestDriver itkNeuralNetworksPrintTest)
itk_add_test(NAME itkNumericsPrintTest
     COMMAND ITKIntegratedTestTestDriver itkNumericsPrintTest)

itk_add_test(NAME itkShrinkImagePreserveObjectPhysicalLocations
      COMMAND ITKIntegratedTestTestDriver itkShrinkImagePreserveObjectPhysicalLocations)
