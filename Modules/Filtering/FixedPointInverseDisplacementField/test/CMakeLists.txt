itk_module_test()

createtestdriver(FixedPointInverseDisplacementField "${FixedPointInverseDisplacementField-Test_LIBRARIES}" "FixedPointInverseDisplacementFieldTest.cxx")

set(
  FixedPointInverseDisplacementFieldTestTestDriverOutput
  ${ITK_TEST_OUTPUT_DIR}/InverseDisplacementField.nii.gz
)

itk_add_test(
  NAME FixedPointInverseDisplacementFieldTest
  COMMAND
    FixedPointInverseDisplacementFieldTestDriver
    --compare
    ${FixedPointInverseDisplacementFieldTestTestDriverOutput}
    DATA{Baseline/InverseDisplacementField.nii.gz}
    FixedPointInverseDisplacementFieldTest
    DATA{Input/DisplacementField.nii.gz}
    ${FixedPointInverseDisplacementFieldTestTestDriverOutput}
)
