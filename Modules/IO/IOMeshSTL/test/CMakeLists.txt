itk_module_test()

set(IOSTLTests itkSTLMeshIOTest.cxx)

createtestdriver(IOSTL "${IOSTL-Test_LIBRARIES}" "${IOSTLTests}" )

itk_add_test(
  NAME itkSTLMeshIOTest00
  COMMAND
    IOSTLTestDriver
    itkSTLMeshIOTest
    DATA{Baseline/sphere.vtk}
    ${ITK_TEST_OUTPUT_DIR}/sphere00.stl
    0 # write in ASCII
)

itk_add_test(
  NAME itkSTLMeshIOTest01
  COMMAND
    IOSTLTestDriver
    itkSTLMeshIOTest
    DATA{Baseline/sphere.vtk}
    ${ITK_TEST_OUTPUT_DIR}/sphere01.stl
    1 # write in BINARY
)

itk_add_test(
  NAME itkSTLMeshIOTest02
  COMMAND
    IOSTLTestDriver
    itkSTLMeshIOTest
    DATA{Baseline/sphere.stl}
    ${ITK_TEST_OUTPUT_DIR}/sphere02.stl
    0 # write in ASCII
)

itk_add_test(
  NAME itkSTLMeshIOTest03
  COMMAND
    IOSTLTestDriver
    itkSTLMeshIOTest
    DATA{Baseline/sphere.stl}
    ${ITK_TEST_OUTPUT_DIR}/sphere03.stl
    1 # write in BINARY
)

itk_add_test(
  NAME itkSTLMeshIOTest04
  COMMAND
    IOSTLTestDriver
    itkSTLMeshIOTest
    DATA{Baseline/tetrahedron.vtk}
    ${ITK_TEST_OUTPUT_DIR}/tetrahedron01.stl
    0 # write in ASCII
)

itk_add_test(
  NAME itkSTLMeshIOTest05
  COMMAND
    IOSTLTestDriver
    itkSTLMeshIOTest
    DATA{Baseline/tetrahedron.vtk}
    ${ITK_TEST_OUTPUT_DIR}/tetrahedron02.stl
    1 # write in BINARY
)

itk_add_test(
  NAME itkSTLMeshIOTest06
  COMMAND
    IOSTLTestDriver
    itkSTLMeshIOTest
    DATA{Baseline/tetrahedron.stl}
    ${ITK_TEST_OUTPUT_DIR}/tetrahedron03.stl
    0 # write in ASCII
)

itk_add_test(
  NAME itkSTLMeshIOTest07
  COMMAND
    IOSTLTestDriver
    itkSTLMeshIOTest
    DATA{Baseline/tetrahedron.stl}
    ${ITK_TEST_OUTPUT_DIR}/tetrahedron04.stl
    1 # write in BINARY
)
