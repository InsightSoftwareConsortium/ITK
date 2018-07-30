itk_module_test()

set(
  BSplineGradientTests
  itkImageToPointSetFilterTest.cxx
  itkImageToImageOfVectorsFilterTest.cxx
  itkBSplineScatteredDataPointSetToGradientImageFilterTest.cxx
)

createtestdriver(BSplineGradient "${BSplineGradient-Test_LIBRARIES}" "${BSplineGradientTests}")

itk_add_test(
  NAME itkImageToPointSetFilterTest
  COMMAND
    BSplineGradientTestDriver
    itkImageToPointSetFilterTest
    DATA{Input/foot.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkImageToPointSetFilterTestOutput.vtk
)

itk_add_test(
  NAME itkImageToImageOfVectorsFilterTest
  COMMAND
    BSplineGradientTestDriver
    itkImageToImageOfVectorsFilterTest
    DATA{Input/foot.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkImageToImageOfVectorsFilterTestOutput.mha
)

itk_add_test(
  NAME itkBSplineScatteredDataPointSetToGradientImageFilterTest
  COMMAND
    BSplineGradientTestDriver
    itkBSplineScatteredDataPointSetToGradientImageFilterTest
    DATA{Input/foot.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredDataPointSetToGradientImageFilterTestGradientMagnitudeOutput.mha
)
