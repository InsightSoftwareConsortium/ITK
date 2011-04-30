itk_module_test()
set(ITK-CurvatureFlowTests
itkBinaryMinMaxCurvatureFlowImageFilterTest.cxx
itkCurvatureFlowHeaderTest.cxx
itkCurvatureFlowTest.cxx
)

CreateTestDriver(ITK-CurvatureFlow  "${ITK-CurvatureFlow-Test_LIBRARIES}" "${ITK-CurvatureFlowTests}")

itk_add_test(NAME itkCurvatureFlowHeaderTest
      COMMAND ITK-CurvatureFlowTestDriver itkCurvatureFlowHeaderTest)
itk_add_test(NAME itkBinaryMinMaxCurvatureFlowImageFilterTest
      COMMAND ITK-CurvatureFlowTestDriver itkBinaryMinMaxCurvatureFlowImageFilterTest)
itk_add_test(NAME itkCurvatureFlowTesti
      COMMAND ITK-CurvatureFlowTestDriver itkCurvatureFlowTest ${ITK_TEST_OUTPUT_DIR}/itkCurvatureFlowTest.vtk)
