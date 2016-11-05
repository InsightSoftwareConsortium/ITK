itk_module_test()
set(ITKAntiAliasTests
itkAntiAliasBinaryImageFilterTest.cxx
)

CreateTestDriver(ITKAntiAlias  "${ITKAntiAlias-Test_LIBRARIES}" "${ITKAntiAliasTests}")

itk_add_test(NAME itkAntiAliasBinaryImageFilterTest
      COMMAND ITKAntiAliasTestDriver
      --compare DATA{Baseline/itkAntiAliasBinaryImageFilterTestBaseline.mha}
                ${ITK_TEST_OUTPUT_DIR}/itkAntiAliasBinaryImageFilterTest.mha
      itkAntiAliasBinaryImageFilterTest
      ${ITK_TEST_OUTPUT_DIR}/itkAntiAliasBinaryImageFilterTest.mha)
