itk_module_test()
set(ITKQuadEdgeMeshTests
itkQuadEdgeMeshPointTest1.cxx
itkQuadEdgeMeshTest1.cxx
itkQuadEdgeMeshTest2.cxx
itkQuadEdgeMeshTest3.cxx
itkQuadEdgeMeshPolygonCellTest.cxx
itkQuadEdgeMeshFrontIteratorTest.cxx
itkQuadEdgeMeshScalarDataVTKPolyDataWriterTest1.cxx
itkQuadEdgeTest1.cxx
itkGeometricalQuadEdgeTest1.cxx
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
itkVTKPolyDataIOQuadEdgeMeshTest.cxx
itkVTKPolyDataReaderQuadEdgeMeshTest.cxx
itkDynamicQuadEdgeMeshTest.cxx
)

CreateTestDriver(ITKQuadEdgeMesh  "${ITKQuadEdgeMesh-Test_LIBRARIES}" "${ITKQuadEdgeMeshTests}")

itk_add_test(NAME itkQuadEdgeMeshPointTest1
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshPointTest1)
itk_add_test(NAME itkQuadEdgeMeshTest1
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshTest1)
itk_add_test(NAME itkQuadEdgeMeshTest2
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshTest2)
itk_add_test(NAME itkQuadEdgeMeshTest3
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshTest3)
itk_add_test(NAME itkQuadEdgeMeshPolygonCellTest
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshPolygonCellTest)
itk_add_test(NAME itkQuadEdgeMeshFrontIteratorTest
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshFrontIteratorTest)
itk_add_test(NAME itkQuadEdgeMeshScalarDataVTKPolyDataWriterTest1
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshScalarDataVTKPolyDataWriterTest1
              ${ITK_TEST_OUTPUT_DIR}/sphere.vtk)
itk_add_test(NAME itkQuadEdgeTest1
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeTest1)
itk_add_test(NAME itkGeometricalQuadEdgeTest1
      COMMAND ITKQuadEdgeMeshTestDriver itkGeometricalQuadEdgeTest1)
itk_add_test(NAME itkQuadEdgeMeshAddFaceTest1_0
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshAddFaceTest1
              0)
itk_add_test(NAME itkQuadEdgeMeshAddFaceTest1_1
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshAddFaceTest1
              1)
itk_add_test(NAME itkQuadEdgeMeshAddFaceTest1_2
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshAddFaceTest1
              2)
itk_add_test(NAME itkQuadEdgeMeshAddFaceTest1_3
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshAddFaceTest1
              3)
itk_add_test(NAME itkQuadEdgeMeshAddFaceTest1_4
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshAddFaceTest1
              4)
itk_add_test(NAME itkQuadEdgeMeshAddFaceTest1_5
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshAddFaceTest1
              5)
itk_add_test(NAME itkQuadEdgeMeshAddFaceTest2
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshAddFaceTest2)
itk_add_test(NAME itkQuadEdgeMeshCellInterfaceTest
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshCellInterfaceTest)
itk_add_test(NAME itkQuadEdgeMeshCountingCellsTest
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshCountingCellsTest)
itk_add_test(NAME itkQuadEdgeMeshDeleteEdgeTest
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshDeleteEdgeTest)
itk_add_test(NAME itkQuadEdgeMeshDeletePointAndReorderIDsTest
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshDeletePointAndReorderIDsTest)
itk_add_test(NAME itkQuadEdgeMeshEulerOperatorCreateCenterVertexTest
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorCreateCenterVertexTest)
itk_add_test(NAME itkQuadEdgeMeshEulerOperatorDeleteCenterVertexTest
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorDeleteCenterVertexTest)
itk_add_test(NAME itkQuadEdgeMeshEulerOperatorFlipTest
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorFlipTest)
itk_add_test(NAME itkQuadEdgeMeshEulerOperatorJoinFacetTest
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorJoinFacetTest)
itk_add_test(NAME itkQuadEdgeMeshEulerOperatorJoinVertexTest0
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorJoinVertexTest
              0)
itk_add_test(NAME itkQuadEdgeMeshEulerOperatorJoinVertexTest1
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorJoinVertexTest
              1)
itk_add_test(NAME itkQuadEdgeMeshEulerOperatorJoinVertexTest2
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorJoinVertexTest
              2)
itk_add_test(NAME itkQuadEdgeMeshEulerOperatorJoinVertexTest3
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorJoinVertexTest
              3)
itk_add_test(NAME itkQuadEdgeMeshEulerOperatorJoinVertexTest4
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorJoinVertexTest
              4)
itk_add_test(NAME itkQuadEdgeMeshEulerOperatorSplitEdgeTest
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorSplitEdgeTest)
itk_add_test(NAME itkQuadEdgeMeshEulerOperatorSplitVertexTest
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshEulerOperatorSplitVertexTest)
itk_add_test(NAME itkQuadEdgeMeshIteratorTest
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshIteratorTest)
itk_add_test(NAME itkQuadEdgeMeshNoPointConstTest
      COMMAND ITKQuadEdgeMeshTestDriver itkQuadEdgeMeshNoPointConstTest)
itk_add_test(NAME itkVTKPolyDataIOQuadEdgeMeshTest
      COMMAND ITKQuadEdgeMeshTestDriver itkVTKPolyDataIOQuadEdgeMeshTest
              DATA{${ITK_DATA_ROOT}/Input/genusZeroSurface01.vtk} ${ITK_TEST_OUTPUT_DIR}/VTKPolyDataIOTest.vtk)
itk_add_test(NAME itkVTKPolyDataReaderQuadEdgeMeshTest
      COMMAND ITKQuadEdgeMeshTestDriver itkVTKPolyDataReaderQuadEdgeMeshTest
              DATA{${ITK_DATA_ROOT}/Input/genusZeroSurface01.vtk})
itk_add_test(NAME itkDynamicQuadEdgeMeshTest
      COMMAND ITKQuadEdgeMeshTestDriver itkDynamicQuadEdgeMeshTest)
