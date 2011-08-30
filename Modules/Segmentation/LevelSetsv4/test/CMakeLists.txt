itk_module_test()
set(ITKLevelSetsv4Tests
itkLevelSetImageBaseTest.cxx
itkWhitakerSparseLevelSetBaseTest.cxx
itkShiSparseLevelSetBaseTest.cxx
itkMalcolmSparseLevelSetBaseTest.cxx
# binary image to sparse level set adaptors
itkBinaryImageToWhitakerSparseLevelSetAdaptorTest.cxx
itkBinaryImageToMalcolmSparseLevelSetAdaptorTest.cxx
itkBinaryImageToShiSparseLevelSetAdaptorTest.cxx
itkLevelSetEquationChanAndVeseExternalTermTest.cxx
itkLevelSetEquationChanAndVeseInternalTermTest.cxx
itkLevelSetEquationCurvatureTermTest.cxx
itkLevelSetEquationLaplacianTermTest.cxx
itkLevelSetEquationPropagationTermTest.cxx
itkLevelSetEquationRegionTermBaseTest.cxx
itkLevelSetEquationTermBaseTest.cxx
itkLevelSetEquationTermContainerBaseTest.cxx
# domain partition classes
itkLevelSetDomainPartitionBaseTest.cxx
itkLevelSetDomainMapImageFilterTest.cxx
# level set container
itkSparseLevelSetContainerTest.cxx
)

CreateTestDriver(ITKLevelSetsv4  "${ITKLevelSetsv4-Test_LIBRARIES}" "${ITKLevelSetsv4Tests}")

itk_add_test(NAME itkLevelSetsv4ImageBaseTest
      COMMAND ITKLevelSetsv4TestDriver itkLevelSetImageBaseTest)
itk_add_test(NAME itkWhitakerSparseLevelSetsv4BaseTest
      COMMAND ITKLevelSetsv4TestDriver itkWhitakerSparseLevelSetBaseTest)
itk_add_test(NAME itkShiSparseLevelSetsv4BaseTest
      COMMAND ITKLevelSetsv4TestDriver itkShiSparseLevelSetBaseTest)
itk_add_test(NAME itkMalcolmSparseLevelSetsv4BaseTest
      COMMAND ITKLevelSetsv4TestDriver itkMalcolmSparseLevelSetBaseTest)
# binary image to sparse level set adaptors
itk_add_test(NAME itkBinaryImageToWhitakerSparseLevelSetsv4AdaptorTest
      COMMAND ITKLevelSetsv4TestDriver
      itkBinaryImageToWhitakerSparseLevelSetAdaptorTest
      ${ITK_DATA_ROOT}/Input/circle.png
      ${ITK_TEST_OUTPUT_DIR}/output_binary_whitaker_adaptor.mha
      ${ITK_TEST_OUTPUT_DIR}/status_binary_whitaker_adaptor.mha
      )
itk_add_test(NAME itkBinaryImageToMalcolmSparseLevelSetsv4AdaptorTest
      COMMAND ITKLevelSetsv4TestDriver
      itkBinaryImageToMalcolmSparseLevelSetAdaptorTest
      ${ITK_DATA_ROOT}/Input/circle.png
      ${ITK_TEST_OUTPUT_DIR}/output_binary_malcolm_adaptor.mha
      )
itk_add_test(NAME itkBinaryImageToShiSparseLevelSetsv4AdaptorTest
      COMMAND ITKLevelSetsv4TestDriver
      itkBinaryImageToShiSparseLevelSetAdaptorTest
      ${ITK_DATA_ROOT}/Input/circle.png
      ${ITK_TEST_OUTPUT_DIR}/output_binary_shi_adaptor.mha
      )
# domain partition classes
itk_add_test(NAME itkLevelSetsv4DomainPartitionBaseTest
      COMMAND ITKLevelSetsv4TestDriver itkLevelSetDomainPartitionBaseTest)
itk_add_test(NAME itkLevelSetsv4DomainMapImageFilterTest
      COMMAND ITKLevelSetsv4TestDriver itkLevelSetDomainMapImageFilterTest)
# level set container
itk_add_test(NAME itkLevelSetsv4ContainerBaseTest
      COMMAND ITKLevelSetsv4TestDriver itkSparseLevelSetContainerTest)
