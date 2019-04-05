itk_module_test()

set(ITKIOMeshGiftiTests
  itkMeshFileReadWriteTest.cxx
)

CreateTestDriver(ITKIOMeshGifti "${ITKIOMeshGifti-Test_LIBRARIES}" "${ITKIOMeshGiftiTests}" )

itk_add_test(NAME itkMeshFileReadWriteTestGifti1
      COMMAND ITKIOMeshGiftiTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/white.gii}
      ${ITK_TEST_OUTPUT_DIR}/white.gii
)
itk_add_test(NAME itkMeshFileReadWriteTestGifti2
      COMMAND ITKIOMeshGiftiTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/aparc.gii}
      ${ITK_TEST_OUTPUT_DIR}/aparc.gii
)
