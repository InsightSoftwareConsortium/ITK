itk_module_test()

set(ITKIOMeshTests
  itkMeshFileReadWriteTest.cxx
  itkMeshFileWriteReadTensorTest.cxx
  itkMeshFileReadWriteVectorAttributeTest.cxx
  itkPolylineReadWriteTest.cxx
)

CreateTestDriver(ITKIOMesh "${ITKIOMesh-Test_LIBRARIES}" "${ITKIOMeshTests}" )

itk_add_test(NAME itkMeshFileReadWriteTest00
      COMMAND ITKIOMeshTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/sphere.vtk}
      ${ITK_TEST_OUTPUT_DIR}/sphere00.vtk
)

itk_add_test(NAME itkMeshFileReadWriteTest01
      COMMAND ITKIOMeshTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/sphere.fsb}
      ${ITK_TEST_OUTPUT_DIR}/sphere01.vtk
)
itk_add_test(NAME itkMeshFileReadWriteTest02
      COMMAND ITKIOMeshTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/sphere.fsb}
      ${ITK_TEST_OUTPUT_DIR}/sphere.fsb
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
itk_add_test(NAME itkMeshFileReadWriteTest05
      COMMAND ITKIOMeshTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/sphere.fsa}
      ${ITK_TEST_OUTPUT_DIR}/sphere.fsa
)

itk_add_test(NAME itkMeshFileReadWriteTest06
      COMMAND ITKIOMeshTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/cube.byu}
      ${ITK_TEST_OUTPUT_DIR}/cube.byu
)
itk_add_test(NAME itkMeshFileReadWriteTest07
      COMMAND ITKIOMeshTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/sphere_curv_b.vtk}
      ${ITK_TEST_OUTPUT_DIR}/sphere_curv_07.vtk
1
)
itk_add_test(NAME itkMeshFileReadWriteTest08
      COMMAND ITKIOMeshTestDriver itkMeshFileReadWriteVectorAttributeTest
      DATA{Baseline/sphere_norm.vtk}
      ${ITK_TEST_OUTPUT_DIR}/sphere_norm.vtk
)
itk_add_test(NAME itkMeshFileReadWriteTest09
      COMMAND ITKIOMeshTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/octa.off}
      ${ITK_TEST_OUTPUT_DIR}/octa.off
)
itk_add_test(NAME itkMeshFileReadWriteTest10
      COMMAND ITKIOMeshTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/box.obj}
      ${ITK_TEST_OUTPUT_DIR}/box.obj
)
itk_add_test(NAME itkMeshFileReadWriteTest11
      COMMAND ITKIOMeshTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/white.gii}
      ${ITK_TEST_OUTPUT_DIR}/white.gii
)
itk_add_test(NAME itkMeshFileReadWriteTest12
      COMMAND ITKIOMeshTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/aparc.gii}
      ${ITK_TEST_OUTPUT_DIR}/aparc.gii
)
itk_add_test(NAME itkPolylineReadWriteTest00
      COMMAND ITKIOMeshTestDriver itkPolylineReadWriteTest
      DATA{Baseline/fibers.vtk}
      ${ITK_TEST_OUTPUT_DIR}/fibers.vtk
)
itk_add_test(NAME itkPolylineReadWriteTest01
      COMMAND ITKIOMeshTestDriver itkPolylineReadWriteTest
      DATA{Baseline/fibers.vtk}
      ${ITK_TEST_OUTPUT_DIR}/fibers_b.vtk
      1
)
itk_add_test(NAME itkMeshFileWriteReadTensorTest
  COMMAND ITKIOMeshTestDriver itkMeshFileWriteReadTensorTest
  ${ITK_TEST_OUTPUT_DIR}/itkMeshFileWriteReadTensorTest2D.vtk
  ${ITK_TEST_OUTPUT_DIR}/itkMeshFileWriteReadTensorTest3D.vtk
)
