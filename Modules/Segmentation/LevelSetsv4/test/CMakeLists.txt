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
