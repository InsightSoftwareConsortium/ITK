itk_module_test()

set(LabelErodeDilateTests
itkLabelSetDilateTest.cxx
itkLabelSetErodeTest.cxx
)

CreateTestDriver(LabelErodeDilate "${LabelErodeDilate-Test_LIBRARIES}" "${LabelErodeDilateTests}")

itk_add_test(NAME itkLabelDilateTest2D_5 
  COMMAND LabelErodeDilateTestDriver 
  --compare DATA{Baseline/axialdilate5.png}
            ${ITK_TEST_OUTPUT_DIR}/axialdilate5.png
  itkLabelSetDilateTest DATA{Input/axial.png} 5 ${ITK_TEST_OUTPUT_DIR}/axialdilate5.png )

itk_add_test(NAME itkLabelDilateTest3D_5 
  COMMAND LabelErodeDilateTestDriver 
  --compare DATA{Baseline/cortdilate_5.nii.gz}
            ${ITK_TEST_OUTPUT_DIR}/cortdilate_5.nii.gz
  itkLabelSetDilateTest DATA{Input/HarvardOxford-cort-maxprob-thr50-1mm.nii.gz} 5 ${ITK_TEST_OUTPUT_DIR}/cortdilate_5.nii.gz )

itk_add_test(NAME itkLabelDilateTest3D_big 
  COMMAND LabelErodeDilateTestDriver 
  --compare DATA{Baseline/dotdilate_41.nii.gz}
            ${ITK_TEST_OUTPUT_DIR}/dotdilate_41.nii.gz
  itkLabelSetDilateTest DATA{Input/dot.nii.gz} 41 ${ITK_TEST_OUTPUT_DIR}/dotdilate_41.nii.gz )

itk_add_test(NAME itkLabelErodeTest2D_3 
  COMMAND LabelErodeDilateTestDriver
  --compare DATA{Baseline/axialerode3.png}
            ${ITK_TEST_OUTPUT_DIR}/axialerode3.png
  itkLabelSetErodeTest DATA{Input/axial.png} 3 ${ITK_TEST_OUTPUT_DIR}/axialerode3.png )

itk_add_test(NAME itkLabelErodeTest3D_3 
  COMMAND LabelErodeDilateTestDriver
  --compare DATA{Baseline/corterode_3.nii.gz}
            ${ITK_TEST_OUTPUT_DIR}/corterode_3.nii.gz
  itkLabelSetErodeTest DATA{Input/HarvardOxford-cort-maxprob-thr50-1mm.nii.gz} 3 ${ITK_TEST_OUTPUT_DIR}/corterode_3.nii.gz )

itk_add_test(NAME itkLabelErodeTest3D_big 
  COMMAND LabelErodeDilateTestDriver
  --compare DATA{Baseline/holeerode_41.nii.gz}
            ${ITK_TEST_OUTPUT_DIR}/holeerode_41.nii.gz
  itkLabelSetErodeTest DATA{Input/hole.nii.gz} 41 ${ITK_TEST_OUTPUT_DIR}/holeerode_41.nii.gz )
