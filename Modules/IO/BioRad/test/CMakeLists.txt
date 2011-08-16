itk_module_test()
set(ITKIOBioRadTests
itkBioRadImageIOTest.cxx
)

CreateTestDriver(ITKIOBioRad  "${ITKIOBioRad-Test_LIBRARIES}" "${ITKIOBioRadTests}")

itk_add_test( NAME ITKBioRadImageIOTest
  COMMAND ITKIOBioRadTestDriver
  --compare DATA{Input/biorad.pic} ${ITK_TEST_OUTPUT_DIR}/biorad_new.pic
  itkBioRadImageIOTest DATA{Input/biorad.pic} ${ITK_TEST_OUTPUT_DIR}/biorad_new.pic )
