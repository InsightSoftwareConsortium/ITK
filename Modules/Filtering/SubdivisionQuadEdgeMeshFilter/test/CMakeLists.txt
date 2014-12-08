itk_module_test()

set(
  itkSubdivisionQuadEdgeMeshFilterTests
  itkCriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest.cxx
  itkCriterionTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest.cxx
  itkTriangleCellSubdivisionQuadEdgeMeshFilterTest.cxx
  itkTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest.cxx
)

createtestdriver(itkSubdivisionQuadEdgeMeshFilter  "${itkSubdivisionQuadEdgeMeshFilter-Test_LIBRARIES}" "${itkSubdivisionQuadEdgeMeshFilterTests}")

set(INPUTDATA ${CMAKE_CURRENT_BINARY_DIR}/data)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

itk_add_test(
  NAME itkModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilterTest0
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_butterfly_0.vtk
    0
    1
)

itk_add_test(
  NAME itkModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilterTest1
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_butterfly_1.vtk
    0
    2
)

itk_add_test(
  NAME itkLinearTriangleCellSubdivisionQuadEdgeMeshFilterTest0
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_linear_0.vtk
    1
    1
)

itk_add_test(
  NAME itkLinearTriangleCellSubdivisionQuadEdgeMeshFilterTest1
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_linear_1.vtk
    1
    2
)

itk_add_test(
  NAME itkLoopTriangleCellSubdivisionQuadEdgeMeshFilterTest0
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_loop_0.vtk
    2
    1
)

itk_add_test(
  NAME itkLoopTriangleCellSubdivisionQuadEdgeMeshFilterTest1
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_loop_1.vtk
    2
    2
)

itk_add_test(
  NAME itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilterTest0
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_square3_0.vtk
    3
    1
)

itk_add_test(
  NAME itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilterTest1
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_square3_1.vtk
    3
    2
)

itk_add_test(
  NAME
    itkAdaptiveModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilterTest0
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_adaptive_butterfly_0.vtk
    0
    1
    1
)

itk_add_test(
  NAME
    itkAdaptiveModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilterTest1
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_adaptive_butterfly_1.vtk
    0
    2
    1
)

itk_add_test(
  NAME itkAdaptiveLinearTriangleCellSubdivisionQuadEdgeMeshFilterTest0
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_adaptive_linear_0.vtk
    1
    1
    1
)

itk_add_test(
  NAME itkAdaptiveLinearTriangleCellSubdivisionQuadEdgeMeshFilterTest1
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_adaptive_linear_1.vtk
    1
    2
    1
)

itk_add_test(
  NAME itkAdaptiveLoopTriangleCellSubdivisionQuadEdgeMeshFilterTest0
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_adaptive_loop_0.vtk
    2
    1
    1
)

itk_add_test(
  NAME itkAdaptiveLoopTriangleCellSubdivisionQuadEdgeMeshFilterTest1
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_adaptive_loop_1.vtk
    2
    2
    1
)

itk_add_test(
  NAME itkAdaptiveSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilterTest0
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_adaptive_square3_0.vtk
    3
    1
    1
)

itk_add_test(
  NAME itkAdaptiveSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilterTest1
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_adaptive_square_1.vtk
    3
    2
    1
)

itk_add_test(
  NAME itkCriterionButterflyTriangleCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkCriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_criterion_butterfly.vtk
    0
    0.01
)

itk_add_test(
  NAME itkCriterionLinearTriangleCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkCriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_criterion_linear.vtk
    1
    0.01
)

itk_add_test(
  NAME itkCriterionLoopTriangleCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkCriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_criterion_loop.vtk
    2
    0.01
)

itk_add_test(
  NAME itkCriterionSquareTriangleCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkCriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_criterion_square3.vtk
    3
    0.01
)

itk_add_test(
  NAME itkButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_edge_butterfly.vtk
    0
    0.1
)

itk_add_test(
  NAME itkLinearTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_edge_linear.vtk
    1
    0.1
)

itk_add_test(
  NAME itkLoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_edge_loop.vtk
    2
    0.01
)

itk_add_test(
  NAME itkCriterionButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkCriterionTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_criterion_edge_butterfly.vtk
    0
    0.05
)

itk_add_test(
  NAME itkCriterionLinearTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkCriterionTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_criterion_edge_linear.vtk
    1
    0.05
)

itk_add_test(
  NAME itkCriterionLoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    itkSubdivisionQuadEdgeMeshFilterTestDriver
    itkCriterionTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_criterion_edge_loop.vtk
    2
    0.05
)
