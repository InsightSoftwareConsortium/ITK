itk_module_test()
set(LabelErodeDilateTests
itkLabelSetDilateTest.cxx
itkLabelSetErodeTest.cxx
)

SET(INPUT_IMAGE2D ${CMAKE_CURRENT_SOURCE_DIR}/images/axial.png)
SET(INPUT_IMAGE3D ${CMAKE_CURRENT_SOURCE_DIR}/images/HarvardOxford-cort-maxprob-thr50-1mm.nii.gz)
SET(INPUT_IMAGE3D_HOLE ${CMAKE_CURRENT_SOURCE_DIR}/images/hole.nii.gz)
SET(INPUT_IMAGE3D_DOT ${CMAKE_CURRENT_SOURCE_DIR}/images/dot.nii.gz)

set(ITK_TEST_DRIVER itkTestDriver)

CreateTestDriver(LabelErodeDilate "${LabelErodeDilate-Test_LIBRARIES}" "${LabelErodeDilateTests}")

itk_add_test(NAME itkLabelDilateTest2D_5 
  COMMAND LabelErodeDilateTestDriver 
  --compare axialdilate5.png ${CMAKE_CURRENT_SOURCE_DIR}/images/baseline/axialdilate5.png
itkLabelSetDilateTest ${INPUT_IMAGE2D} 5 axialdilate5.png )

itk_add_test(NAME itkLabelDilateTest3D_5 
  COMMAND LabelErodeDilateTestDriver 
  --compare cortdilate_5.nii.gz ${CMAKE_CURRENT_SOURCE_DIR}/images/baseline/cortdilate_5.nii.gz
itkLabelSetDilateTest ${INPUT_IMAGE3D} 5 cortdilate_5.nii.gz )

itk_add_test(NAME itkLabelDilateTest3D_big 
  COMMAND LabelErodeDilateTestDriver 
  --compare dotdilate_41.nii.gz ${CMAKE_CURRENT_SOURCE_DIR}/images/baseline/dotdilate_41.nii.gz
itkLabelSetDilateTest ${INPUT_IMAGE3D_DOT} 41 dotdilate_41.nii.gz )

itk_add_test(NAME itkLabelErodeTest2D_3 
  COMMAND LabelErodeDilateTestDriver
  --compare axialerode3.png ${CMAKE_CURRENT_SOURCE_DIR}/images/baseline/axialerode3.png
itkLabelSetErodeTest ${INPUT_IMAGE2D} 3 axialerode3.png )

itk_add_test(NAME itkLabelErodeTest3D_3 
  COMMAND LabelErodeDilateTestDriver
  --compare corterode_3.nii.gz ${CMAKE_CURRENT_SOURCE_DIR}/images/baseline/corterode_3.nii.gz
  itkLabelSetErodeTest ${INPUT_IMAGE3D} 3 corterode_3.nii.gz )

itk_add_test(NAME itkLabelErodeTest3D_big 
  COMMAND LabelErodeDilateTestDriver
  --compare holeerode_41.nii.gz ${CMAKE_CURRENT_SOURCE_DIR}/images/baseline/holeerode_41.nii.gz
itkLabelSetErodeTest ${INPUT_IMAGE3D_HOLE} 41 holeerode_41.nii.gz )
