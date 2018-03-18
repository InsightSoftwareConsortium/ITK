itk_module_test()

set(ITKIOMeshBYUTests
  itkMeshFileReadWriteTest.cxx
)

CreateTestDriver(ITKIOMeshBYU "${ITKIOMesh-Test_LIBRARIES}" "${ITKIOMeshBYUTests}" )

itk_add_test(NAME itkMeshFileReadWriteTest06
    COMMAND ITKIOMeshBYUTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/cube.byu}
      ${ITK_TEST_OUTPUT_DIR}/cube.byu
)
