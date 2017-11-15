itk_module_test()

set(PolarTransformTests itkPolarTransformTest.cxx)

createtestdriver(PolarTransform "${PolarTransform-Test_LIBRARIES}" "${PolarTransformTests}")

itk_add_test(
  NAME itkPolarTransformTest
  COMMAND
    PolarTransformTestDriver
    itkPolarTransformTest
)
