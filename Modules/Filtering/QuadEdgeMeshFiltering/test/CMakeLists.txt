itk_module_test()
set(ITK-QuadEdgeMeshFilteringTests
itkAutomaticTopologyQuadEdgeMeshSourceTest.cxx
itkBinaryMask3DQuadEdgeMeshSourceTest.cxx
itkCleanQuadEdgeMeshFilterTest.cxx
itkDelaunayConformingQuadEdgeMeshFilterTest.cxx
itkDiscreteGaussianCurvatureQuadEdgeMeshFilterTest.cxx
itkDiscreteMaximumCurvatureQuadEdgeMeshFilterTest.cxx
itkDiscreteMeanCurvatureQuadEdgeMeshFilterTest.cxx
itkDiscreteMinimumCurvatureQuadEdgeMeshFilterTest.cxx
itkNormalQuadEdgeMeshFilterTest.cxx
itkParameterizationQuadEdgeMeshFilterTest.cxx
itkQuadEdgeMeshFilteringHeaderTest.cxx
itkQuadricDecimationQuadEdgeMeshFilterTest.cxx
itkRegularSphereQuadEdgeMeshSourceTest.cxx
itkSmoothingQuadEdgeMeshFilterTest.cxx
itkSquaredEdgeLengthDecimationQuadEdgeMeshFilterTest.cxx
)


CreateTestDriver(ITK-QuadEdgeMeshFiltering  "${ITK-QuadEdgeMeshFiltering-Test_LIBRARIES}" "${ITK-QuadEdgeMeshFilteringTests}")

set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

add_test(NAME itkSquaredEdgeLengthDecimationQuadEdgeMeshFilterTest
      COMMAND ITK-QuadEdgeMeshFilteringTestDriver
          itkSquaredEdgeLengthDecimationQuadEdgeMeshFilterTest
          ${INPUTDATA}/mushroom.vtk 20 ${TEMP}/temp_SquaredEdgeLengthDecimationResult1.vtk)

add_test(NAME itkSmoothingQuadEdgeMeshFilterTest0
      COMMAND ITK-QuadEdgeMeshFilteringTestDriver
          itkSmoothingQuadEdgeMeshFilterTest
          ${INPUTDATA}/genusZeroSurface01.vtk 10 0.1 0 ${TEMP}/temp_SmoothResult0.vtk)

add_test(NAME itkSmoothingQuadEdgeMeshFilterTest1
      COMMAND ITK-QuadEdgeMeshFilteringTestDriver
             itkSmoothingQuadEdgeMeshFilterTest
          ${INPUTDATA}/genusZeroSurface01.vtk 10 0.1 1 ${TEMP}/temp_SmoothResult1.vtk)

set( CURV_TESTS Gaussian Maximum Mean Minimum )
foreach( loop_var ${CURV_TESTS} )
  add_test(NAME itkDiscrete${loop_var}CurvatureQuadEdgeMeshFilterTest
      COMMAND ITK-QuadEdgeMeshFilteringTestDriver
            itkDiscrete${loop_var}CurvatureQuadEdgeMeshFilterTest
            ${INPUTDATA}/mushroom.vtk)
endforeach( loop_var )

add_test(NAME itkDelaunayConformingQuadEdgeMeshFilterTest
      COMMAND ITK-QuadEdgeMeshFilteringTestDriver
          itkDelaunayConformingQuadEdgeMeshFilterTest ${INPUTDATA}/mushroom.vtk ${TEMP}/mushrom_delaunay.vtk)
add_test(NAME itkCleanQuadEdgeMeshFilterTest
      COMMAND ITK-QuadEdgeMeshFilteringTestDriver
          itkCleanQuadEdgeMeshFilterTest
          ${INPUTDATA}/mushroom.vtk 0.1 ${TEMP}/temp_CleanResult1.vtk)
 add_test(NAME itkQuadricDecimationQuadEdgeMeshFilterTest
      COMMAND ITK-QuadEdgeMeshFilteringTestDriver
              itkQuadricDecimationQuadEdgeMeshFilterTest
              ${INPUTDATA}/mushroom.vtk 100 ${TEMP}/temp_QuadricDecimationResult1.vtk)
add_test(NAME itkQuadEdgeMeshQuadricDecimationTetrahedronTest
      COMMAND ITK-QuadEdgeMeshFilteringTestDriver
              itkQuadricDecimationQuadEdgeMeshFilterTest
              ${INPUTDATA}/tetrahedron.vtk 2 ${TEMP}/temp_QuadricDecimationTetrahedron.vtk)
add_test(NAME itkQuadEdgeMeshFilteringHeaderTest
      COMMAND ITK-QuadEdgeMeshFilteringTestDriver itkQuadEdgeMeshFilteringHeaderTest)
add_test(NAME itkAutomaticTopologyQuadEdgeMeshSourceTest
      COMMAND ITK-QuadEdgeMeshFilteringTestDriver itkAutomaticTopologyQuadEdgeMeshSourceTest)
add_test(NAME itkBinaryMask3DQuadEdgeMeshSourceTest
      COMMAND ITK-QuadEdgeMeshFilteringTestDriver itkBinaryMask3DQuadEdgeMeshSourceTest)
add_test(NAME itkRegularSphereQuadEdgeMeshSourceTest
      COMMAND ITK-QuadEdgeMeshFilteringTestDriver itkRegularSphereQuadEdgeMeshSourceTest
              ${ITK_TEST_OUTPUT_DIR}/itkRegularSphereMeshQuadEdgeMeshSourceTest.vtk)
add_test(NAME itkNormalQuadEdgeMeshFilterTest0
      COMMAND ITK-QuadEdgeMeshFilteringTestDriver itkNormalQuadEdgeMeshFilterTest
              ${INPUTDATA}/mushroom.vtk 0)
add_test(NAME itkNormalQuadEdgeMeshFilterTest1
      COMMAND ITK-QuadEdgeMeshFilteringTestDriver itkNormalQuadEdgeMeshFilterTest
              ${INPUTDATA}/mushroom.vtk 1)
add_test(NAME itkNormalQuadEdgeMeshFilterTest2
      COMMAND ITK-QuadEdgeMeshFilteringTestDriver itkNormalQuadEdgeMeshFilterTest
              ${INPUTDATA}/mushroom.vtk 2)

foreach( border 0 1 )
  foreach( coeff 0 1 2 3 4 )
    add_test(NAME itkParameterizationQuadEdgeMeshFilterTest${border}_${coeff}
          COMMAND ITK-QuadEdgeMeshFilteringTestDriver
                  itkParameterizationQuadEdgeMeshFilterTest
            ${INPUTDATA}/mushroom.vtk ${border} ${coeff}
            ${TEMP}/param_border${border}_coeff${coeff}.vtk)
  endforeach( coeff )
endforeach( border )
