itk_module_test()
set(ITK-ClassifiersTests
itkClassifiersHeaderTest.cxx
itkScalarImageKmeansImageFilterTest.cxx
)

CreateTestDriver(ITK-Classifiers  "${ITK-Classifiers-Test_LIBRARIES}" "${ITK-ClassifiersTests}")

add_test(NAME itkClassifiersHeaderTest
      COMMAND ITK-ClassifiersTestDriver itkClassifiersHeaderTest)
