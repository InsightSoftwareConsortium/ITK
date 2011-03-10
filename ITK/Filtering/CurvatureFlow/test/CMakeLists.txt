itk_module_test()
set(ITK-CurvatureFlowTests
itkBinaryMinMaxCurvatureFlowImageFilterTest.cxx
itkCurvatureFlowHeaderTest.cxx
)

CreateTestDriver(ITK-CurvatureFlow  "${ITK-CurvatureFlow-Test_LIBRARIES}" "${ITK-CurvatureFlowTests}")

add_test(NAME itkCurvatureFlowHeaderTest
      COMMAND ITK-CurvatureFlowTestDriver itkCurvatureFlowHeaderTest)
add_test(NAME itkBinaryMinMaxCurvatureFlowImageFilterTest
      COMMAND ITK-CurvatureFlowTestDriver itkBinaryMinMaxCurvatureFlowImageFilterTest)
