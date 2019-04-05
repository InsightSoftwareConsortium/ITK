itk_module_test()

set(ITKIOMeshOFFTests
  itkMeshFileReadWriteTest.cxx
)

CreateTestDriver(ITKIOMeshOFF "${ITKIOMeshOFF-Test_LIBRARIES}" "${ITKIOMeshOFFTests}" )

itk_add_test(NAME itkMeshFileReadWriteOFFTest
      COMMAND ITKIOMeshTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/octa.off}
      ${ITK_TEST_OUTPUT_DIR}/octa.off
)
