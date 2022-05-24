itk_module_test()

set(ITKIOMeshBaseTests
  itkMeshFileReaderWriterTest.cxx
)

CreateTestDriver(ITKIOMeshBase "${ITKIOMeshBase-Test_LIBRARIES}" "${ITKIOMeshBaseTests}")

itk_add_test(NAME itkMeshFileReaderWriterTest
      COMMAND ITKIOMeshBaseTestDriver itkMeshFileReaderWriterTest
      DATA{${ITK_DATA_ROOT}/Input/mushroom.vtk}
      ${ITK_TEST_OUTPUT_DIR}/itkMeshFileReaderWriterTest.vtk
      DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha}
)
