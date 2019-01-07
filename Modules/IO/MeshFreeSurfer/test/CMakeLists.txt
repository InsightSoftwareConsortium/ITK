itk_module_test()

set(ITKIOMeshFreeSurferTests
  itkMeshFileReadWriteTest.cxx
)

CreateTestDriver(ITKIOMeshFreeSurfer "${ITKIOMeshFreeSurfer-Test_LIBRARIES}" "${ITKIOMeshFreeSurferTests}" )

itk_add_test(NAME itkMeshFileReadWriteTestFreeSurfer1
  COMMAND ITKIOMeshFreeSurferTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/sphere.fsb}
      ${ITK_TEST_OUTPUT_DIR}/sphere.fsb
)
itk_add_test(NAME itkMeshFileReadWriteTestFreeSurfer2
    COMMAND ITKIOMeshFreeSurferTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/sphere.fsa}
      ${ITK_TEST_OUTPUT_DIR}/sphere.fsa
)
