itk_module_test()

set(BSplineGradientTests itkImageToPointSetFilterTest.cxx)

createtestdriver(BSplineGradient "${BSplineGradient-Test_LIBRARIES}" "${BSplineGradientTests}")

itk_add_test(
  NAME itkImageToPointSetFilterTest
  COMMAND
    BSplineGradientTestDriver
    itkImageToPointSetFilterTest
    DATA{Input/foot.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkImageToPointSetFilterTestOutput.vtk
)
