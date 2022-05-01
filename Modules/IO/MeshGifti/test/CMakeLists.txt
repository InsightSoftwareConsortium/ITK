itk_module_test()

set(ITKIOMeshGiftiTests
  itkGiftiMeshIOTest.cxx
  itkMeshFileReadWriteTest.cxx
)

CreateTestDriver(ITKIOMeshGifti "${ITKIOMeshGifti-Test_LIBRARIES}" "${ITKIOMeshGiftiTests}")

itk_add_test(NAME itkGiftiMeshIOTest1
      COMMAND ITKIOMeshGiftiTestDriver itkGiftiMeshIOTest
      DATA{Baseline/aparc.gii}
      ${ITK_TEST_OUTPUT_DIR}/giftimeshioaparc.gii
      DATA{${ITK_DATA_ROOT}/Input/tetrahedron.vtk}
      ${ITK_TEST_OUTPUT_DIR}/gifti2vtkaparc.vtk
      0 1 1 1 1 0 0 1 0 131122 0 0 1
)
itk_add_test(NAME itkGiftiMeshIOTest2
      COMMAND ITKIOMeshGiftiTestDriver itkGiftiMeshIOTest
      DATA{Baseline/white.gii}
      ${ITK_TEST_OUTPUT_DIR}/giftimeshiowhite.gii
      DATA{${ITK_DATA_ROOT}/Input/tetrahedron.vtk}
      ${ITK_TEST_OUTPUT_DIR}/gifti2vtkwhite.vtk
      0 1 1 1 1 0 0 1 22134 0 44264 0 0
)
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
