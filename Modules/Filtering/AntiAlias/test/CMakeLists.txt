itk_module_test()
set(ITK-AntiAliasTests
itkAntiAliasHeaderTest.cxx
itkAntiAliasBinaryImageFilterTest.cxx
)

CreateTestDriver(ITK-AntiAlias  "${ITK-AntiAlias-Test_LIBRARIES}" "${ITK-AntiAliasTests}")

itk_add_test(NAME itkAntiAliasHeaderTest
      COMMAND ITK-AntiAliasTestDriver itkAntiAliasHeaderTest)
itk_add_test(NAME itkAntiAliasBinaryImageFilterTest
      COMMAND ITK-AntiAliasTestDriver itkAntiAliasBinaryImageFilterTest)
