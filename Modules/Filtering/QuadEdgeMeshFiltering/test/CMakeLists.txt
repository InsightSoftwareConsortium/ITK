itk_module_test()
set(ITKQuadEdgeMeshFilteringTests
itkAutomaticTopologyQuadEdgeMeshSourceTest.cxx
itkBorderQuadEdgeMeshFilterTest.cxx
itkBorderQuadEdgeMeshFilterTest2.cxx
itkBinaryMask3DQuadEdgeMeshSourceTest.cxx
itkCleanQuadEdgeMeshFilterTest.cxx
itkDelaunayConformingQuadEdgeMeshFilterTest.cxx
itkDiscreteGaussianCurvatureQuadEdgeMeshFilterTest.cxx
itkDiscreteMaximumCurvatureQuadEdgeMeshFilterTest.cxx
itkDiscreteMeanCurvatureQuadEdgeMeshFilterTest.cxx
itkDiscreteMinimumCurvatureQuadEdgeMeshFilterTest.cxx
itkNormalQuadEdgeMeshFilterTest.cxx
itkParameterizationQuadEdgeMeshFilterTest.cxx
itkQuadricDecimationQuadEdgeMeshFilterTest.cxx
itkRegularSphereQuadEdgeMeshSourceTest.cxx
itkSmoothingQuadEdgeMeshFilterTest.cxx
itkSquaredEdgeLengthDecimationQuadEdgeMeshFilterTest.cxx
itkLaplacianDeformationQuadEdgeMeshFilterWithSoftConstraintsTest.cxx
itkLaplacianDeformationQuadEdgeMeshFilterWithHardConstraintsTest.cxx
)

CreateTestDriver(ITKQuadEdgeMeshFiltering  "${ITKQuadEdgeMeshFiltering-Test_LIBRARIES}" "${ITKQuadEdgeMeshFilteringTests}")

set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

itk_add_test(NAME itkBorderQuadEdgeMeshFilterTest_0_0
      COMMAND ITKQuadEdgeMeshFilteringTestDriver
              itkBorderQuadEdgeMeshFilterTest
            DATA{${INPUTDATA}/mushroom.vtk} 0 0 ${TEMP}/border00.vtk
)

itk_add_test(NAME itkBorderQuadEdgeMeshFilterTest_0_1
      COMMAND ITKQuadEdgeMeshFilteringTestDriver
              itkBorderQuadEdgeMeshFilterTest
            DATA{${INPUTDATA}/mushroom.vtk} 0 1 ${TEMP}/border01.vtk
)

itk_add_test(NAME itkBorderQuadEdgeMeshFilterTest_1_0
      COMMAND ITKQuadEdgeMeshFilteringTestDriver
              itkBorderQuadEdgeMeshFilterTest
            DATA{${INPUTDATA}/mushroom.vtk} 1 0 ${TEMP}/border10.vtk
)

itk_add_test(NAME itkBorderQuadEdgeMeshFilterTest_1_1
      COMMAND ITKQuadEdgeMeshFilteringTestDriver
              itkBorderQuadEdgeMeshFilterTest
            DATA{${INPUTDATA}/mushroom.vtk} 1 1 ${TEMP}/border11.vtk
)

itk_add_test(NAME itkBorderQuadEdgeMeshFilterTest2_0
      COMMAND ITKQuadEdgeMeshFilteringTestDriver
              itkBorderQuadEdgeMeshFilterTest2 0
)

itk_add_test(NAME itkBorderQuadEdgeMeshFilterTest2_1
      COMMAND ITKQuadEdgeMeshFilteringTestDriver
              itkBorderQuadEdgeMeshFilterTest2 1
)
itk_add_test(NAME itkSquaredEdgeLengthDecimationQuadEdgeMeshFilterTest
      COMMAND ITKQuadEdgeMeshFilteringTestDriver
          itkSquaredEdgeLengthDecimationQuadEdgeMeshFilterTest
          DATA{${INPUTDATA}/mushroom.vtk} 20 ${TEMP}/temp_SquaredEdgeLengthDecimationResult1.vtk)

itk_add_test(NAME itkSmoothingQuadEdgeMeshFilterTest0
      COMMAND ITKQuadEdgeMeshFilteringTestDriver
          itkSmoothingQuadEdgeMeshFilterTest
          DATA{${INPUTDATA}/genusZeroSurface01.vtk} 10 0.1 0 ${TEMP}/temp_SmoothResult0.vtk)

itk_add_test(NAME itkSmoothingQuadEdgeMeshFilterTest1
      COMMAND ITKQuadEdgeMeshFilteringTestDriver
             itkSmoothingQuadEdgeMeshFilterTest
          DATA{${INPUTDATA}/genusZeroSurface01.vtk} 10 0.1 1 ${TEMP}/temp_SmoothResult1.vtk)

set( CURV_TESTS Gaussian Maximum Mean Minimum )
foreach( loop_var ${CURV_TESTS} )
  itk_add_test(NAME itkDiscrete${loop_var}CurvatureQuadEdgeMeshFilterTest
      COMMAND ITKQuadEdgeMeshFilteringTestDriver
            itkDiscrete${loop_var}CurvatureQuadEdgeMeshFilterTest
            DATA{${INPUTDATA}/mushroom.vtk})
endforeach()

itk_add_test(NAME itkDelaunayConformingQuadEdgeMeshFilterTest
      COMMAND ITKQuadEdgeMeshFilteringTestDriver
          itkDelaunayConformingQuadEdgeMeshFilterTest DATA{${INPUTDATA}/mushroom.vtk} ${TEMP}/mushrom_delaunay.vtk)
itk_add_test(NAME itkCleanQuadEdgeMeshFilterTest
      COMMAND ITKQuadEdgeMeshFilteringTestDriver
          itkCleanQuadEdgeMeshFilterTest
          DATA{${INPUTDATA}/mushroom.vtk} 0.1 ${TEMP}/temp_CleanResult1.vtk)
 itk_add_test(NAME itkQuadricDecimationQuadEdgeMeshFilterTest
      COMMAND ITKQuadEdgeMeshFilteringTestDriver
              itkQuadricDecimationQuadEdgeMeshFilterTest
              DATA{${INPUTDATA}/mushroom.vtk} 100 ${TEMP}/temp_QuadricDecimationResult1.vtk)
itk_add_test(NAME itkQuadEdgeMeshQuadricDecimationTetrahedronTest
      COMMAND ITKQuadEdgeMeshFilteringTestDriver
              itkQuadricDecimationQuadEdgeMeshFilterTest
              DATA{${INPUTDATA}/tetrahedron.vtk} 2 ${TEMP}/temp_QuadricDecimationTetrahedron.vtk)
itk_add_test(NAME itkAutomaticTopologyQuadEdgeMeshSourceTest
      COMMAND ITKQuadEdgeMeshFilteringTestDriver itkAutomaticTopologyQuadEdgeMeshSourceTest)
itk_add_test(NAME itkBinaryMask3DQuadEdgeMeshSourceTest
      COMMAND ITKQuadEdgeMeshFilteringTestDriver itkBinaryMask3DQuadEdgeMeshSourceTest)
itk_add_test(NAME itkRegularSphereQuadEdgeMeshSourceTest
      COMMAND ITKQuadEdgeMeshFilteringTestDriver itkRegularSphereQuadEdgeMeshSourceTest
              ${ITK_TEST_OUTPUT_DIR}/itkRegularSphereMeshQuadEdgeMeshSourceTest.vtk)
itk_add_test(NAME itkNormalQuadEdgeMeshFilterTest0
      COMMAND ITKQuadEdgeMeshFilteringTestDriver itkNormalQuadEdgeMeshFilterTest
              DATA{${INPUTDATA}/mushroom.vtk} 0)
itk_add_test(NAME itkNormalQuadEdgeMeshFilterTest1
      COMMAND ITKQuadEdgeMeshFilteringTestDriver itkNormalQuadEdgeMeshFilterTest
              DATA{${INPUTDATA}/mushroom.vtk} 1)
itk_add_test(NAME itkNormalQuadEdgeMeshFilterTest2
      COMMAND ITKQuadEdgeMeshFilteringTestDriver itkNormalQuadEdgeMeshFilterTest
              DATA{${INPUTDATA}/mushroom.vtk} 2)

foreach( area 0 1 )
  itk_add_test(NAME itkLaplacianDeformationQuadEdgeMeshFilterWithSoftConstraintsArea${area}Test
        COMMAND ITKQuadEdgeMeshFilteringTestDriver itkLaplacianDeformationQuadEdgeMeshFilterWithSoftConstraintsTest
                DATA{${INPUTDATA}/genusZeroSurface01.vtk} ${TEMP}/LaplacianDeformationSoftConstraintsOutput_area${area}.vtk ${area})
  itk_add_test(NAME itkLaplacianDeformationQuadEdgeMeshFilterWithHardConstraintsArea${area}Test
        COMMAND ITKQuadEdgeMeshFilteringTestDriver itkLaplacianDeformationQuadEdgeMeshFilterWithHardConstraintsTest
                DATA{${INPUTDATA}/genusZeroSurface01.vtk} ${TEMP}/LaplacianDeformationHardConstraintsOutput_area${area}.vtk ${area})
endforeach()

foreach( border 0 1 )
  foreach( coeff 0 1 2 3 4 )
    foreach( solver 0 1 )
      itk_add_test(NAME itkParameterizationQuadEdgeMeshFilterTestb${border}_c${coeff}_s${solver}
            COMMAND ITKQuadEdgeMeshFilteringTestDriver
                    itkParameterizationQuadEdgeMeshFilterTest
              DATA{${INPUTDATA}/mushroom.vtk} ${border} ${coeff} ${solver}
              ${TEMP}/param_border${border}_coeff${coeff}_solver${solver}.vtk)
    endforeach()
  endforeach()
endforeach()
