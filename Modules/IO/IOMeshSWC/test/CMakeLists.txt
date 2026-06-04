itk_module_test()

set(IOMeshSWCTests itkMeshFileReadWriteTest.cxx)

createtestdriver(IOMeshSWC "${IOMeshSWC-Test_LIBRARIES}" "${IOMeshSWCTests}" )

itk_add_test(
  NAME itkMeshFileReadSWCTest
  COMMAND
    IOMeshSWCTestDriver
    itkMeshFileReadWriteTest
    DATA{Input/17109_4101-X6753-Y6197_reg.swc}
    ${ITK_TEST_OUTPUT_DIR}/17109_4101-X6753-Y6197_reg.swc.vtk
)

itk_add_test(
  NAME itkMeshFileReadWriteSWCTest
  COMMAND
    IOMeshSWCTestDriver
    itkMeshFileReadWriteTest
    DATA{Input/17109_4101-X6753-Y6197_reg.swc}
    ${ITK_TEST_OUTPUT_DIR}/17109_4101-X6753-Y6197_reg.swc
)

itk_add_test(
  NAME itkMeshFileReadSWCTest2
  COMMAND
    IOMeshSWCTestDriver
    itkMeshFileReadWriteTest
    DATA{Input/18453_3564-X30226-Y9677_reg.swc}
    ${ITK_TEST_OUTPUT_DIR}/18453_3564-X30226-Y9677_reg.swc.vtk
)

itk_add_test(
  NAME itkMeshFileReadWriteSWCTest2
  COMMAND
    IOMeshSWCTestDriver
    itkMeshFileReadWriteTest
    DATA{Input/18453_3564-X30226-Y9677_reg.swc}
    ${ITK_TEST_OUTPUT_DIR}/18453_3564-X30226-Y9677_reg.swc
)

itk_add_test(
  NAME itkMeshFileReadSWCTest3
  COMMAND
    IOMeshSWCTestDriver
    itkMeshFileReadWriteTest
    DATA{Input/11706c2.CNG.swc}
    ${ITK_TEST_OUTPUT_DIR}/11706c2.CNG.swc.vtk
)

itk_add_test(
  NAME itkMeshFileReadWriteSWCTest3
  COMMAND
    IOMeshSWCTestDriver
    itkMeshFileReadWriteTest
    DATA{Input/11706c2.CNG.swc}
    ${ITK_TEST_OUTPUT_DIR}/11706c2.CNG.swc
)

set(IOMeshSWCGTests itkSWCMeshIOGTest.cxx)

creategoogletestdriver(IOMeshSWC "${IOMeshSWC-Test_LIBRARIES}" "${IOMeshSWCGTests}")
