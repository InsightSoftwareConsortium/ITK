itk_module_test()
set(ITK-AntiAliasTests
itkAntiAliasHeaderTest.cxx
itkAntiAliasBinaryImageFilterTest.cxx
)

CreateTestDriver(ITK-AntiAlias  "${ITK-AntiAlias-Test_LIBRARIES}" "${ITK-AntiAliasTests}")

add_test(NAME itkAntiAliasHeaderTest
      COMMAND ITK-AntiAliasTestDriver itkAntiAliasHeaderTest)
add_test(NAME itkAntiAliasBinaryImageFilterTest
      COMMAND ITK-AntiAliasTestDriver itkAntiAliasBinaryImageFilterTest)
