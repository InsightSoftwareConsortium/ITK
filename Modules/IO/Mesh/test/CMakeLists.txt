itk_module_test()

set(ITKIOMeshTests
  itkMeshFileReadWriteTest.cxx
)

CreateTestDriver(ITKIOMesh "${ITKIOMesh-Test_LIBRARIES}" "${ITKIOMeshTests}" )

itk_add_test(NAME itkMeshFileReadWriteTest01
      COMMAND ITKIOMeshTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/sphere.fsb}
      ${ITK_TEST_OUTPUT_DIR}/sphere01.vtk
)
itk_add_test(NAME itkMeshFileReadWriteTest03
      COMMAND ITKIOMeshTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/sphere_curv.vtk}
      ${ITK_TEST_OUTPUT_DIR}/sphere_curv_03.vtk
)
itk_add_test(NAME itkMeshFileReadWriteTest04
      COMMAND ITKIOMeshTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/thickness.fcv}
      ${ITK_TEST_OUTPUT_DIR}/thickness.fcv
)
itk_add_test(NAME itkMeshFileReadWriteTest07
      COMMAND ITKIOMeshTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/sphere_curv_b.vtk}
      ${ITK_TEST_OUTPUT_DIR}/sphere_curv_07.vtk
      1
)
