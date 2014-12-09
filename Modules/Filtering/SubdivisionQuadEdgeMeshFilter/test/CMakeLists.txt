itk_module_test()

set(
  SubdivisionQuadEdgeMeshFilterTests
  itkCriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest.cxx
  itkCriterionTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest.cxx
  itkTriangleCellSubdivisionQuadEdgeMeshFilterTest.cxx
  itkTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest.cxx
)

createtestdriver(SubdivisionQuadEdgeMeshFilter  "${SubdivisionQuadEdgeMeshFilter-Test_LIBRARIES}" "${SubdivisionQuadEdgeMeshFilterTests}")

set(INPUTDATA ${CMAKE_CURRENT_SOURCE_DIR}/data)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

itk_add_test(
  NAME itkModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilterTest0
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_butterfly_0.vtk
    0
    1
)

itk_add_test(
  NAME itkModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilterTest1
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_butterfly_1.vtk
    0
    2
)

itk_add_test(
  NAME itkLinearTriangleCellSubdivisionQuadEdgeMeshFilterTest0
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_linear_0.vtk
    1
    1
)

itk_add_test(
  NAME itkLinearTriangleCellSubdivisionQuadEdgeMeshFilterTest1
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_linear_1.vtk
    1
    2
)

itk_add_test(
  NAME itkLoopTriangleCellSubdivisionQuadEdgeMeshFilterTest0
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_loop_0.vtk
    2
    1
)

itk_add_test(
  NAME itkLoopTriangleCellSubdivisionQuadEdgeMeshFilterTest1
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_loop_1.vtk
    2
    2
)

itk_add_test(
  NAME itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilterTest0
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_square3_0.vtk
    3
    1
)

itk_add_test(
  NAME itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilterTest1
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
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
    SubdivisionQuadEdgeMeshFilterTestDriver
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
    SubdivisionQuadEdgeMeshFilterTestDriver
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
    SubdivisionQuadEdgeMeshFilterTestDriver
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
    SubdivisionQuadEdgeMeshFilterTestDriver
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
    SubdivisionQuadEdgeMeshFilterTestDriver
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
    SubdivisionQuadEdgeMeshFilterTestDriver
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
    SubdivisionQuadEdgeMeshFilterTestDriver
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
    SubdivisionQuadEdgeMeshFilterTestDriver
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
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkCriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_criterion_butterfly.vtk
    0
    0.01
)

itk_add_test(
  NAME itkCriterionLinearTriangleCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkCriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_criterion_linear.vtk
    1
    0.01
)

itk_add_test(
  NAME itkCriterionLoopTriangleCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkCriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_criterion_loop.vtk
    2
    0.01
)

itk_add_test(
  NAME itkCriterionSquareTriangleCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkCriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_criterion_square3.vtk
    3
    0.01
)

itk_add_test(
  NAME itkButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_edge_butterfly.vtk
    0
    0.1
)

itk_add_test(
  NAME itkLinearTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_edge_linear.vtk
    1
    0.1
)

itk_add_test(
  NAME itkLoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_edge_loop.vtk
    2
    0.01
)

itk_add_test(
  NAME itkCriterionButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkCriterionTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_criterion_edge_butterfly.vtk
    0
    0.05
)

itk_add_test(
  NAME itkCriterionLinearTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkCriterionTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_criterion_edge_linear.vtk
    1
    0.05
)

itk_add_test(
  NAME itkCriterionLoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkCriterionTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
    DATA{${INPUTDATA}/venus.vtk}
    ${TEMP}/venus_criterion_edge_loop.vtk
    2
    0.05
)
