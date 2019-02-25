itk_module_test()

set(ITKIOMeshVTKTests
  itkMeshFileReadWriteTest.cxx
  itkMeshFileWriteReadTensorTest.cxx
  itkMeshFileReadWriteVectorAttributeTest.cxx
  itkPolylineReadWriteTest.cxx
  itkVTKPolyDataMeshCanReadImageTest.cxx
)

CreateTestDriver(ITKIOMeshVTK "${ITKIOMeshVTK-Test_LIBRARIES}" "${ITKIOMeshVTKTests}" )

itk_add_test(NAME itkMeshFileReadWriteTest00
      COMMAND ITKIOMeshVTKTestDriver itkMeshFileReadWriteTest
      DATA{Baseline/sphere.vtk}
      ${ITK_TEST_OUTPUT_DIR}/sphere00.vtk
)
itk_add_test(NAME itkMeshFileReadWriteVectorAttributeTest
      COMMAND ITKIOMeshVTKTestDriver itkMeshFileReadWriteVectorAttributeTest
      DATA{Baseline/sphere_norm.vtk}
      ${ITK_TEST_OUTPUT_DIR}/sphere_norm.vtk
)
itk_add_test(NAME itkPolylineReadWriteTest00
      COMMAND ITKIOMeshVTKTestDriver itkPolylineReadWriteTest
      DATA{Baseline/fibers.vtk}
      ${ITK_TEST_OUTPUT_DIR}/fibers.vtk
)
itk_add_test(NAME itkPolylineReadWriteTest01
      COMMAND ITKIOMeshVTKTestDriver itkPolylineReadWriteTest
      DATA{Baseline/fibers.vtk}
      ${ITK_TEST_OUTPUT_DIR}/fibers_b.vtk
      1
)
itk_add_test(NAME itkMeshFileWriteReadTensorTest
  COMMAND ITKIOMeshVTKTestDriver itkMeshFileWriteReadTensorTest
  ${ITK_TEST_OUTPUT_DIR}/itkMeshFileWriteReadTensorTest2D.vtk
  ${ITK_TEST_OUTPUT_DIR}/itkMeshFileWriteReadTensorTest3D.vtk
)
itk_add_test(NAME itkVTKPolyDataMeshCanReadImageTest
      COMMAND ITKIOMeshVTKTestDriver itkVTKPolyDataMeshCanReadImageTest
      DATA{Input/ironProt.vtk}
)
