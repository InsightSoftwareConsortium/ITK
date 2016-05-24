itk_module_test()

set(GenericLabelInterpolatorTests
  RotateLabels.cxx
  )

CreateTestDriver(GenericLabelInterpolator "${GenericLabelInterpolator-Test_LIBRARIES}" "${GenericLabelInterpolatorTests}")

itk_add_test(NAME GenericLabelInterpolatorRotateLabelsTest
  COMMAND GenericLabelInterpolatorTestDriver
  --compare
    ${ITK_TEST_OUTPUT_DIR}/gl_gaussian_3.mha
    DATA{Baseline/gl_gaussian_3.mha}
  RotateLabels
    DATA{Input/classification.mha}
    ${ITK_TEST_OUTPUT_DIR}/
    3
  )
