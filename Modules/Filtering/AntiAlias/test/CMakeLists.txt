itk_module_test()
set(ITKAntiAliasTests
itkAntiAliasHeaderTest.cxx
itkAntiAliasBinaryImageFilterTest.cxx
)

CreateTestDriver(ITKAntiAlias  "${ITKAntiAlias-Test_LIBRARIES}" "${ITKAntiAliasTests}")

itk_add_test(NAME itkAntiAliasHeaderTest
      COMMAND ITKAntiAliasTestDriver itkAntiAliasHeaderTest)
itk_add_test(NAME itkAntiAliasBinaryImageFilterTest
      COMMAND ITKAntiAliasTestDriver itkAntiAliasBinaryImageFilterTest)
