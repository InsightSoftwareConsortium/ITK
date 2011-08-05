itk_module_test()
set(ITKAntiAliasTests
itkAntiAliasBinaryImageFilterTest.cxx
)

CreateTestDriver(ITKAntiAlias  "${ITKAntiAlias-Test_LIBRARIES}" "${ITKAntiAliasTests}")

itk_add_test(NAME itkAntiAliasBinaryImageFilterTest
      COMMAND ITKAntiAliasTestDriver itkAntiAliasBinaryImageFilterTest)
