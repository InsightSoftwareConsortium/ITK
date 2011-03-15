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
itkQuadEdgeMeshAddFaceTest1.cxx
itkQuadEdgeMeshAddFaceTest2.cxx
itkQuadEdgeMeshBasicLayerTest.cxx
itkQuadEdgeMeshCellInterfaceTest.cxx
itkQuadEdgeMeshCountingCellsTest.cxx
itkQuadEdgeMeshDeleteEdgeTest.cxx
itkQuadEdgeMeshDeletePointAndReorderIDsTest.cxx
itkQuadEdgeMeshEulerOperatorCreateCenterVertexTest.cxx
itkQuadEdgeMeshEulerOperatorDeleteCenterVertexTest.cxx
itkQuadEdgeMeshEulerOperatorFlipTest.cxx
itkQuadEdgeMeshEulerOperatorJoinFacetTest.cxx
itkQuadEdgeMeshEulerOperatorJoinVertexTest.cxx
itkQuadEdgeMeshEulerOperatorSplitEdgeTest.cxx
itkQuadEdgeMeshEulerOperatorSplitFaceTest.cxx
itkQuadEdgeMeshEulerOperatorSplitVertexTest.cxx
itkQuadEdgeMeshIteratorTest.cxx
itkQuadEdgeMeshNoPointConstTest.cxx
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
add_test(NAME itkQuadEdgeMeshAddFaceTest1_0
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshAddFaceTest1
              0)
add_test(NAME itkQuadEdgeMeshAddFaceTest1_1
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshAddFaceTest1
              1)
add_test(NAME itkQuadEdgeMeshAddFaceTest1_2
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshAddFaceTest1
              2)
add_test(NAME itkQuadEdgeMeshAddFaceTest1_3
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshAddFaceTest1
              3)
add_test(NAME itkQuadEdgeMeshAddFaceTest1_4
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshAddFaceTest1
              4)
add_test(NAME itkQuadEdgeMeshAddFaceTest1_5
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshAddFaceTest1
              5)
add_test(NAME itkQuadEdgeMeshAddFaceTest2
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshAddFaceTest2)
add_test(NAME itkQuadEdgeMeshCellInterfaceTest
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshCellInterfaceTest)
add_test(NAME itkQuadEdgeMeshCountingCellsTest
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshCountingCellsTest)
add_test(NAME itkQuadEdgeMeshDeleteEdgeTest
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshDeleteEdgeTest)
add_test(NAME itkQuadEdgeMeshDeletePointAndReorderIDsTest
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshDeletePointAndReorderIDsTest)
add_test(NAME itkQuadEdgeMeshEulerOperatorCreateCenterVertexTest
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorCreateCenterVertexTest)
add_test(NAME itkQuadEdgeMeshEulerOperatorDeleteCenterVertexTest
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorDeleteCenterVertexTest)
add_test(NAME itkQuadEdgeMeshEulerOperatorFlipTest
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorFlipTest)
add_test(NAME itkQuadEdgeMeshEulerOperatorJoinFacetTest
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorJoinFacetTest)
add_test(NAME itkQuadEdgeMeshEulerOperatorJoinVertexTest0
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorJoinVertexTest
              0)
add_test(NAME itkQuadEdgeMeshEulerOperatorJoinVertexTest1
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorJoinVertexTest
              1)
add_test(NAME itkQuadEdgeMeshEulerOperatorJoinVertexTest2
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorJoinVertexTest
              2)
add_test(NAME itkQuadEdgeMeshEulerOperatorJoinVertexTest3
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorJoinVertexTest
              3)
add_test(NAME itkQuadEdgeMeshEulerOperatorJoinVertexTest4
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorJoinVertexTest
              4)
add_test(NAME itkQuadEdgeMeshEulerOperatorSplitEdgeTest
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorSplitEdgeTest)
add_test(NAME itkQuadEdgeMeshEulerOperatorSplitVertexTest
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorSplitVertexTest)
add_test(NAME itkQuadEdgeMeshIteratorTest
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshIteratorTest)
add_test(NAME itkQuadEdgeMeshNoPointConstTest
      COMMAND ITK-QuadEdgeMeshTestDriver itkQuadEdgeMeshNoPointConstTest)
