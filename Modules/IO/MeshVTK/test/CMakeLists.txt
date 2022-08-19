itk_module_test()

set(ITKIOMeshVTKTests
  itkMeshFileReadWriteTest.cxx
  itkMeshFileWriteReadTensorTest.cxx
  itkMeshFileReadWriteVectorAttributeTest.cxx
  itkPolylineReadWriteTest.cxx
  itkVTKPolyDataMeshCanReadImageTest.cxx
  itkVTKPolyDataMeshIOTest.cxx
)

CreateTestDriver(ITKIOMeshVTK "${ITKIOMeshVTK-Test_LIBRARIES}" "${ITKIOMeshVTKTests}")

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
itk_add_test(NAME itkPolyLineReadWriteTest00
      COMMAND ITKIOMeshVTKTestDriver itkPolylineReadWriteTest
      DATA{Baseline/fibers.vtk}
      ${ITK_TEST_OUTPUT_DIR}/fibers.vtk
)
itk_add_test(NAME itkPolyLineReadWriteTest01
      COMMAND ITKIOMeshVTKTestDriver itkPolylineReadWriteTest
      DATA{Baseline/fibers.vtk}
      ${ITK_TEST_OUTPUT_DIR}/fibers_b.vtk
      1
)
itk_add_test(NAME itkPolyLineReadWriteTest02
      COMMAND ITKIOMeshVTKTestDriver itkPolylineReadWriteTest
      DATA{Baseline/hollow_test.vtk}
      ${ITK_TEST_OUTPUT_DIR}/hollow_test.vtk
)
itk_add_test(NAME itkPolyLineReadWriteTest03
      COMMAND ITKIOMeshVTKTestDriver itkPolylineReadWriteTest
      DATA{Baseline/hollow_test.vtk}
      ${ITK_TEST_OUTPUT_DIR}/hollow_test_b.vtk
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
itk_add_test(NAME itkVTKPolyDataMeshIOTestInOutAscii
      COMMAND ITKIOMeshVTKTestDriver itkVTKPolyDataMeshIOTest
      DATA{Baseline/sphere.vtk}
      ${ITK_TEST_OUTPUT_DIR}/sphere_a.vtk
      DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha}
      ${ITK_TEST_OUTPUT_DIR}/ascii2asciivtk2mhaHeadMRVolume.mha
      0 1 1 1 1 162 0 320 0 0 0
)
itk_add_test(NAME itkVTKPolyDataMeshIOTestInAsciiOutBinary
      COMMAND ITKIOMeshVTKTestDriver itkVTKPolyDataMeshIOTest
      DATA{Baseline/sphere.vtk}
      ${ITK_TEST_OUTPUT_DIR}/sphere_b.vtk
      DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha}
      ${ITK_TEST_OUTPUT_DIR}/ascii2binaryvtk2mhaHeadMRVolume.mha
      0 1 1 1 1 162 0 320 0 0 1
)
itk_add_test(NAME itkVTKPolyDataMeshIOTestInBinaryOutAscii
      COMMAND ITKIOMeshVTKTestDriver itkVTKPolyDataMeshIOTest
      DATA{Baseline/fibers.vtk}
      ${ITK_TEST_OUTPUT_DIR}/fibers_a.vtk
      DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha}
      ${ITK_TEST_OUTPUT_DIR}/binary2asciivtk2mhaHeadMRVolume.mha
      0 1 1 1 1 72 72 2 2 1 0
)
itk_add_test(NAME itkVTKPolyDataMeshIOTestInOutBinary
      COMMAND ITKIOMeshVTKTestDriver itkVTKPolyDataMeshIOTest
      DATA{Baseline/fibers.vtk}
      ${ITK_TEST_OUTPUT_DIR}/fibers_b.vtk
      DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha}
      ${ITK_TEST_OUTPUT_DIR}/binary2binaryvtk2mhaHeadMRVolume.mha
      0 1 1 1 1 72 72 2 2 1 1
)

set(ITKIOMeshVTKGTests
  itkVTKPolyDataMeshIOGTest.cxx
)

CreateGoogleTestDriver(ITKIOMeshVTK "${ITKIOMeshVTK-Test_LIBRARIES}" "${ITKIOMeshVTKGTests}")
