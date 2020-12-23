itk_module_test()

set(ITKIOMeshBaseTests
  itkMeshFileReaderTest.cxx
)

CreateTestDriver(ITKIOMeshBase "${ITKIOMeshBase-Test_LIBRARIES}" "${ITKIOMeshBaseTests}" )

itk_add_test(NAME itkMeshFileReaderTest
      COMMAND ITKIOMeshBaseTestDriver itkMeshFileReaderTest
      DATA{${ITK_DATA_ROOT}/Input/mushroom.vtk}
)
