itk_module_test()

createtestdriver(FixedPointInverseDeformationField "${FixedPointInverseDeformationField-Test_LIBRARIES}" "FixedPointInverseDeformationFieldTest.cxx")

set(
  FixedPointInverseDeformationFieldTestTestDriverOutput
  ${ITK_TEST_OUTPUT_DIR}/InverseDeformationField.nii.gz
)

itk_add_test(
  NAME FixedPointInverseDeformationFieldTest
  COMMAND
    FixedPointInverseDeformationFieldTestDriver
    --compare
    ${FixedPointInverseDeformationFieldTestTestDriverOutput}
    DATA{Baseline/InverseDeformationField.nii.gz}
    FixedPointInverseDeformationFieldTest
    DATA{Input/DeformationField.nii.gz}
    ${FixedPointInverseDeformationFieldTestTestDriverOutput}
)
