itk_module_test()

set(
  ${kit}-TestDriver_LIBRARIES
  ${ITK_LIBRARIES}
  ${kit}Lib
)
set(TestDriverAdditionalSource ${kit}EntryPoint.cxx)

createtestdriver(${kit} "${${kit}-TestDriver_LIBRARIES}" "${TestDriverAdditionalSource}")

set(
  ${kit}TestDriverOutput
  ${ITK_TEST_OUTPUT_DIR}/InverseDeformationField.nii.gz
)
itk_add_test(
  NAME ${kit}Test
  COMMAND
    ${kit}TestDriver
    --compare
    ${${kit}TestDriverOutput}
    DATA{Baseline/InverseDeformationField.nii.gz}
    ${kit}EntryPoint
    DATA{Input/DeformationField.nii.gz}
    ${${kit}TestDriverOutput}
)
