itk_module_test()
set(ITKCurvatureFlowTests
itkBinaryMinMaxCurvatureFlowImageFilterTest.cxx
itkCurvatureFlowHeaderTest.cxx
itkCurvatureFlowTest.cxx
)

CreateTestDriver(ITKCurvatureFlow  "${ITKCurvatureFlow-Test_LIBRARIES}" "${ITKCurvatureFlowTests}")

itk_add_test(NAME itkCurvatureFlowHeaderTest
      COMMAND ITKCurvatureFlowTestDriver itkCurvatureFlowHeaderTest)
itk_add_test(NAME itkBinaryMinMaxCurvatureFlowImageFilterTest
      COMMAND ITKCurvatureFlowTestDriver itkBinaryMinMaxCurvatureFlowImageFilterTest)
itk_add_test(NAME itkCurvatureFlowTesti
      COMMAND ITKCurvatureFlowTestDriver itkCurvatureFlowTest ${ITK_TEST_OUTPUT_DIR}/itkCurvatureFlowTest.vtk)
