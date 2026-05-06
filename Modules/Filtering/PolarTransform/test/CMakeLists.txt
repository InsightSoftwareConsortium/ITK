itk_module_test()

set(PolarTransformTests itkPolarTransformTest.cxx)

createtestdriver(PolarTransform "${PolarTransform-Test_LIBRARIES}" "${PolarTransformTests}")

itk_add_test(
  NAME itkPolarTransformTest
  COMMAND
    PolarTransformTestDriver
    itkPolarTransformTest
)

if(ITK_WRAP_PYTHON)
  itk_python_expression_add_test(
    NAME itkPolarToCartesianTransformPythonTest
    EXPRESSION "instance = itk.PolarToCartesianTransform.New()"
  )
  itk_python_expression_add_test(
    NAME itkCartesianToPolarTransformPythonTest
    EXPRESSION "instance = itk.CartesianToPolarTransform.New()"
  )
endif()
