itk_module_test()

set(
  SubdivisionQuadEdgeMeshFilterTests
  itkCriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest.cxx
  itkCriterionTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest.cxx
  itkSubdivisionQuadEdgeMeshFilterRegressionTest.cxx
  itkTriangleCellSubdivisionQuadEdgeMeshFilterTest.cxx
  itkTriangleCellSubdivisionQuadEdgeMeshFilterCellDataTest.cxx
  itkTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest.cxx
)

createtestdriver(SubdivisionQuadEdgeMeshFilter  "${SubdivisionQuadEdgeMeshFilter-Test_LIBRARIES}" "${SubdivisionQuadEdgeMeshFilterTests}")

set(INPUTDATA ${CMAKE_CURRENT_SOURCE_DIR}/data)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

# Test TriangleCellSubdivision Filters

foreach(METHOD 0 1 2 3)
  itk_add_test(
    NAME itkCriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest${METHOD}
    COMMAND
      SubdivisionQuadEdgeMeshFilterTestDriver
      itkCriterionTriangleCellSubdivisionQuadEdgeMeshFilterTest
      DATA{${INPUTDATA}/venus.vtk}
      ${TEMP}/venus_criterion_${METHOD}.vtk
      ${METHOD}
      0.01
  )

  foreach(RESOLUTION 1 2)
    itk_add_test(
      NAME
        itkAdaptiveTriangleCellSubdivisionQuadEdgeMeshFilterTest${METHOD}${RESOLUTION}
      COMMAND
        SubdivisionQuadEdgeMeshFilterTestDriver
        itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
        DATA{${INPUTDATA}/venus.vtk}
        ${TEMP}/venus_adaptive_${METHOD}${RESOLUTION}.vtk
        ${METHOD}
        ${RESOLUTION}
    )

    foreach(ADAPTIVE 0 1)
      itk_add_test(
        NAME
          itkAdaptiveTriangleCellSubdivisionQuadEdgeMeshFilterTest${METHOD}${RESOLUTION}${ADAPTIVE}
        COMMAND
          SubdivisionQuadEdgeMeshFilterTestDriver
          itkTriangleCellSubdivisionQuadEdgeMeshFilterTest
          DATA{${INPUTDATA}/venus.vtk}
          ${TEMP}/venus_adaptive_${METHOD}${RESOLUTION}${ADAPTIVE}.vtk
          ${METHOD}
          ${RESOLUTION}
          ${ADAPTIVE}
      )
    endforeach()
  endforeach()
endforeach()

# Test TriangleEdgeCellSubdivision Filters

foreach(METHOD 0 1 2)
  itk_add_test(
    NAME itkTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest${METHOD}
    COMMAND
      SubdivisionQuadEdgeMeshFilterTestDriver
      itkTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
      DATA{${INPUTDATA}/venus.vtk}
      ${TEMP}/venus_edge_${METHOD}.vtk
      ${METHOD}
      0.1
  )

  itk_add_test(
    NAME itkCriterionTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest${METHOD}
    COMMAND
      SubdivisionQuadEdgeMeshFilterTestDriver
      itkCriterionTriangleEdgeCellSubdivisionQuadEdgeMeshFilterTest
      DATA{${INPUTDATA}/venus.vtk}
      ${TEMP}/venus_criterion_edge_${METHOD}.vtk
      ${METHOD}
      0.05
  )
endforeach()

# Test whether CellData is properly passed

itk_add_test(
  NAME itkTriangleCellSubdivisionQuadEdgeMeshFilterCellDataTest
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkTriangleCellSubdivisionQuadEdgeMeshFilterCellDataTest
)

# Regressions for the smoothedPointSet cell-id-vs-point-id bug and the
# infinite-loop on non-triangle cells. TIMEOUT guards against a future
# regression that re-introduces the hang.
itk_add_test(
  NAME itkSubdivisionQuadEdgeMeshFilterRegressionTest
  COMMAND
    SubdivisionQuadEdgeMeshFilterTestDriver
    itkSubdivisionQuadEdgeMeshFilterRegressionTest
)
set_tests_properties(
  itkSubdivisionQuadEdgeMeshFilterRegressionTest
  PROPERTIES
    TIMEOUT
      30
)
