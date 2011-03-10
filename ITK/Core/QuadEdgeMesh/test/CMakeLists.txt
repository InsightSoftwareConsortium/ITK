itk_module_test()
set(ITK-QuadEdgeMeshTests
itkQuadEdgeMeshPointTest1.cxx
itkQuadEdgeMeshTest1.cxx
itkQuadEdgeMeshTest2.cxx
itkQuadEdgeMeshTest3.cxx
itkQuadEdgeMeshPolygonCellTest.cxx
itkQuadEdgeMeshFrontIteratorTest.cxx
itkQuadEdgeMeshScalarDataVTKPolyDataWriterTest1.cxx
itkQuadEdgeTest1.cxx
itkGeometricalQuadEdgeTest1.cxx
itkQuadEdgeMeshHeaderTest.cxx
)

CreateTestDriver(ITK-QuadEdgeMesh  "${ITK-QuadEdgeMesh-Test_LIBRARIES}" "${ITK-QuadEdgeMeshTests}")

add_test(NAME itkQuadEdgeMeshHeaderTest
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshHeaderTest)
add_test(NAME itkQuadEdgeMeshPointTest1
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshPointTest1)
add_test(NAME itkQuadEdgeMeshTest1
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshTest1)
add_test(NAME itkQuadEdgeMeshTest2
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshTest2)
add_test(NAME itkQuadEdgeMeshTest3
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshTest3)
add_test(NAME itkQuadEdgeMeshPolygonCellTest
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshPolygonCellTest)
add_test(NAME itkQuadEdgeMeshFrontIteratorTest
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshFrontIteratorTest)
add_test(NAME itkQuadEdgeMeshScalarDataVTKPolyDataWriterTest1
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshScalarDataVTKPolyDataWriterTest1
              ${ITK_TEST_OUTPUT_DIR}/sphere.vtk)
add_test(NAME itkQuadEdgeTest1
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeTest1)
add_test(NAME itkGeometricalQuadEdgeTest1
      COMMAND ITK-QuadEdgeMeshTestDriver itkGeometricalQuadEdgeTest1)
