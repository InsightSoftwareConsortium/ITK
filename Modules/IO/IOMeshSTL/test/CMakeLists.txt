itk_module_test()

set(IOMeshSTLTests itkSTLMeshIOTest.cxx)

createtestdriver(IOMeshSTL "${IOMeshSTL-Test_LIBRARIES}" "${IOMeshSTLTests}" )

itk_add_test(
  NAME itkSTLMeshIOTest00
  COMMAND
    IOMeshSTLTestDriver
    itkSTLMeshIOTest
    DATA{Baseline/sphere.vtk}
    ${ITK_TEST_OUTPUT_DIR}/sphere00.stl
    0 # write in ASCII
)

itk_add_test(
  NAME itkSTLMeshIOTest01
  COMMAND
    IOMeshSTLTestDriver
    itkSTLMeshIOTest
    DATA{Baseline/sphere.vtk}
    ${ITK_TEST_OUTPUT_DIR}/sphere01.stl
    1 # write in BINARY
)

itk_add_test(
  NAME itkSTLMeshIOTest02
  COMMAND
    IOMeshSTLTestDriver
    itkSTLMeshIOTest
    DATA{Baseline/sphere.stl}
    ${ITK_TEST_OUTPUT_DIR}/sphere02.stl
    0 # write in ASCII
)

itk_add_test(
  NAME itkSTLMeshIOTest03
  COMMAND
    IOMeshSTLTestDriver
    itkSTLMeshIOTest
    DATA{Baseline/sphere.stl}
    ${ITK_TEST_OUTPUT_DIR}/sphere03.stl
    1 # write in BINARY
)

itk_add_test(
  NAME itkSTLMeshIOTest04
  COMMAND
    IOMeshSTLTestDriver
    itkSTLMeshIOTest
    DATA{Baseline/tetrahedron.vtk}
    ${ITK_TEST_OUTPUT_DIR}/tetrahedron01.stl
    0 # write in ASCII
)

itk_add_test(
  NAME itkSTLMeshIOTest05
  COMMAND
    IOMeshSTLTestDriver
    itkSTLMeshIOTest
    DATA{Baseline/tetrahedron.vtk}
    ${ITK_TEST_OUTPUT_DIR}/tetrahedron02.stl
    1 # write in BINARY
)

itk_add_test(
  NAME itkSTLMeshIOTest06
  COMMAND
    IOMeshSTLTestDriver
    itkSTLMeshIOTest
    DATA{Baseline/tetrahedron.stl}
    ${ITK_TEST_OUTPUT_DIR}/tetrahedron03.stl
    0 # write in ASCII
)

itk_add_test(
  NAME itkSTLMeshIOTest07
  COMMAND
    IOMeshSTLTestDriver
    itkSTLMeshIOTest
    DATA{Baseline/tetrahedron.stl}
    ${ITK_TEST_OUTPUT_DIR}/tetrahedron04.stl
    1 # write in BINARY
)
