itk_module_test()

#${itk-module} will be the name of this module and will not need to be
#changed when this module is renamed.

set(
  ${itk-module}Tests
  itkBinaryThinningImageFilter3DTest.cxx
  itkMedialThicknessImageFilter3DTest.cxx
)

createtestdriver(${itk-module}  "${${itk-module}-Test_LIBRARIES}" "${${itk-module}Tests}")

itk_add_test(
  NAME itkBinaryThinningImageFilter3DTest
  COMMAND
    ${itk-module}TestDriver
    --compare
    DATA{Baseline/skeleton.tiff}
    ${ITK_TEST_OUTPUT_DIR}/skeleton.tiff
    itkBinaryThinningImageFilter3DTest
    DATA{Input/input.tiff}
    ${ITK_TEST_OUTPUT_DIR}/skeleton.tiff
)

itk_add_test(
  NAME itkMedialThicknessImageFilter3DTest
  COMMAND
    ${itk-module}TestDriver
    --compare
    DATA{Baseline/medial_thickness.tiff}
    ${ITK_TEST_OUTPUT_DIR}/medial_thickness.tiff
    itkMedialThicknessImageFilter3DTest
    DATA{Input/input.tiff}
    ${ITK_TEST_OUTPUT_DIR}/medial_thickness.tiff
)
