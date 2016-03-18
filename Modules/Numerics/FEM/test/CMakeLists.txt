itk_module_test()

set(ITKFEMTests
itkFEMExceptionTest.cxx
itkFEMElement2DMembraneTest.cxx
itkFEMElement3DMembraneTest.cxx
itkFEMElement2DStrainTest.cxx
itkFEMElement2DQuadraticTriangularTest.cxx
itkFEMLinearSystemWrapperItpackTest.cxx
itkFEMLinearSystemWrapperItpackTest2.cxx
itkFEMLinearSystemWrapperVNLTest.cxx
itkFEMLinearSystemWrapperDenseVNLTest.cxx
itkFEMPArrayTest.cxx
itkFEMElement2DC0LinearTriangleStressTest.cxx
itkFEMElement2DC0LinearQuadrilateralStrainItpackTest.cxx
itkFEMElement2DC0LinearTriangleStrainTest.cxx
itkFEMElement2DC0LinearTriangleMembraneTest.cxx
itkFEMElement2DC0LinearQuadrilateralStressTest.cxx
itkFEMElement2DC0LinearQuadrilateralStrainTest.cxx
itkFEMElement2DC0LinearQuadrilateralMembraneTest.cxx
itkFEMElement2DC0QuadraticTriangleStrainTest.cxx
itkFEMElement2DC0QuadraticTriangleStressTest.cxx
itkFEMElement2DC0LinearLineStressTest.cxx
itkFEMElement2DC1BeamTest.cxx
itkFEMElement3DC0LinearHexahedronStrainTest.cxx
itkFEMElement3DC0LinearHexahedronMembraneTest.cxx
itkFEMElement3DC0LinearTetrahedronStrainTest.cxx
itkFEMElement3DC0LinearTetrahedronMembraneTest.cxx
itkFEMLoadBCMFCTest.cxx
itkFEMLoadBCMFCTestUser.cxx
itkFEMLoadEdgeTest.cxx
itkFEMLoadGravConstTest.cxx
itkFEMLoadPointTestUser.cxx
itkFEMLandmarkLoadImplementationTest.cxx
# itkFEMSolverTest2D.cxx
itkFEMSolverTest3D.cxx
itkImageToRectilinearFEMObjectFilter2DTest.cxx
itkImageToRectilinearFEMObjectFilter3DTest.cxx
itkFEMElement2DTest.cxx
itkFEMElement3DTest.cxx
itkFEMObjectTest.cxx
itkFEMObjectTest2.cxx
itkFEMSpatialObjectTest.cxx
itkFEMSolverHyperbolicTest.cxx
itkFEMRobustSolverTest.cxx
itkFEMScatteredDataPointSetToImageFilterTest.cxx
)

CreateTestDriver(ITKFEM  "${ITKFEM-Test_LIBRARIES}" "${ITKFEMTests}")

itk_add_test(NAME itkFEMElement2DMembraneTest
      COMMAND ITKFEMTestDriver itkFEMElement2DMembraneTest)
itk_add_test(NAME itkFEMElement2DQuadraticTriangularTest
      COMMAND ITKFEMTestDriver itkFEMElement2DQuadraticTriangularTest)
itk_add_test(NAME itkFEMElement2DStrainTest
      COMMAND ITKFEMTestDriver itkFEMElement2DStrainTest)
itk_add_test(NAME itkFEMElement3DMembraneTest
      COMMAND ITKFEMTestDriver itkFEMElement3DMembraneTest)
itk_add_test(NAME itkFEMExceptionTest
      COMMAND ITKFEMTestDriver itkFEMExceptionTest)
itk_add_test(NAME itkFEMLinearSystemWrapperDenseVNLTest
      COMMAND ITKFEMTestDriver itkFEMLinearSystemWrapperDenseVNLTest)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest
      COMMAND ITKFEMTestDriver itkFEMLinearSystemWrapperItpackTest)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest1
      COMMAND ITKFEMTestDriver itkFEMLinearSystemWrapperItpackTest
              0)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest2
      COMMAND ITKFEMTestDriver itkFEMLinearSystemWrapperItpackTest
              1)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest3
      COMMAND ITKFEMTestDriver itkFEMLinearSystemWrapperItpackTest
              2)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest4
      COMMAND ITKFEMTestDriver itkFEMLinearSystemWrapperItpackTest
              3)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest5
      COMMAND ITKFEMTestDriver itkFEMLinearSystemWrapperItpackTest
              4)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest6
      COMMAND ITKFEMTestDriver itkFEMLinearSystemWrapperItpackTest
              5)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest7
      COMMAND ITKFEMTestDriver itkFEMLinearSystemWrapperItpackTest
              6)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest8
      COMMAND ITKFEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              0)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest9
      COMMAND ITKFEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              1)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest10
      COMMAND ITKFEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              2)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest11
      COMMAND ITKFEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              3)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest12
      COMMAND ITKFEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              4)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest13
      COMMAND ITKFEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              5)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest14
      COMMAND ITKFEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              6)
itk_add_test(NAME itkFEMLinearSystemWrapperVNLTest
      COMMAND ITKFEMTestDriver itkFEMLinearSystemWrapperVNLTest)
itk_add_test(NAME itkFEMPArrayTest
      COMMAND ITKFEMTestDriver itkFEMPArrayTest)
itk_add_test(NAME itkFEMScatteredDataPointSetToImageFilterTest
      COMMAND ITKFEMTestDriver itkFEMScatteredDataPointSetToImageFilterTest)


# 3D Element Tests
itk_add_test(NAME itkFEMC0HexahedralElement-NodalLoads-BCs
      COMMAND ITKFEMTestDriver itkFEMElement3DTest
              DATA{Input/hexa2.meta})
itk_add_test(NAME itkFEMC0HexahedralElement-NoLoads
      COMMAND ITKFEMTestDriver itkFEMElement3DTest
              DATA{Input/hexa3.meta})
itk_add_test(NAME itkFEMC0HexahedralElement-GravityLoad-BCs
      COMMAND ITKFEMTestDriver itkFEMElement3DTest
              DATA{Input/hexa4-grav.meta})
itk_add_test(NAME itkFEMC0TetrahedralElement-NodalLoads-BCs
      COMMAND ITKFEMTestDriver itkFEMElement3DTest
              DATA{Input/tetra2.meta})
itk_add_test(NAME itkFEMC0TetrahedralElement-NoLoads
      COMMAND ITKFEMTestDriver itkFEMElement3DTest
              DATA{Input/tetra3.meta})
itk_add_test(NAME itkFEMC0TetrahedralElement-GravityLoad-BCs
      COMMAND ITKFEMTestDriver itkFEMElement3DTest
              DATA{Input/tetra4-grav.meta})


# 2D Element Tests
itk_add_test(NAME itkFEMC0QuadElement-NodalLoads-BCs
      COMMAND ITKFEMTestDriver itkFEMElement2DTest
              DATA{Input/quad2-small.meta})
itk_add_test(NAME itkFEMC0QuadElement-Strain
      COMMAND ITKFEMTestDriver itkFEMElement2DTest
              DATA{Input/quad2-strain.meta})
itk_add_test(NAME itkFEMC0QuadElement-NoLoads
      COMMAND ITKFEMTestDriver itkFEMElement2DTest
              DATA{Input/quad4.meta})
itk_add_test(NAME itkFEMC0QuadElement-GravityLoad-BCs
      COMMAND ITKFEMTestDriver itkFEMElement2DTest
              DATA{Input/quad6-grav.meta})
itk_add_test(NAME itkFEMLoadLandmarkImplementation
      COMMAND ITKFEMTestDriver itkFEMElement2DTest
              DATA{Input/quad-lm.meta})
itk_add_test(NAME itkFEMC0QuadElementTrapezoidalGeometry-NoLoads
      COMMAND ITKFEMTestDriver itkFEMElement2DTest
              DATA{Input/trapezoid.meta})
itk_add_test(NAME itkFEMC0TriangularElement-NodalLoads-BCs
      COMMAND ITKFEMTestDriver itkFEMElement2DTest
              DATA{Input/tri2.meta})
itk_add_test(NAME itkFEMC0TriangularElement-NoLoads
      COMMAND ITKFEMTestDriver itkFEMElement2DTest
              DATA{Input/tri3.meta})
itk_add_test(NAME itkFEMC0TriangularElement-Strain
      COMMAND ITKFEMTestDriver itkFEMElement2DTest
              DATA{Input/tri3-e.meta})
itk_add_test(NAME itkFEMC0TriangularElement-Quadratic
      COMMAND ITKFEMTestDriver itkFEMElement2DTest
              DATA{Input/tri3-q.meta})
itk_add_test(NAME itkFEMTruss
      COMMAND ITKFEMTestDriver itkFEMElement2DTest
              DATA{Input/truss.meta})
itk_add_test(NAME itkFEMSolverHyperbolicTest
      COMMAND ITKFEMTestDriver itkFEMSolverHyperbolicTest
              DATA{Input/quad2-small.meta} 5 0 0.0 0.0 5.9473e-07 -2.41038e-06
              3.88745e-06 -2.64591e-06 0.0 0.0)

itk_add_test(NAME itkFEMElement2DC0LinearQuadrilateralStrainTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMElement2DC0LinearQuadrilateralStrainTest
         DATA{Input/2DC0LinearQuadrilateralStrainTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC0LinearQuadrilateralStrainTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC0LinearQuadrilateralStressTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMElement2DC0LinearQuadrilateralStressTest
         ${ITK_TEST_OUTPUT_DIR}/2DC0LinearQuadrilateralStressTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC0LinearQuadrilateralMembraneTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMElement2DC0LinearQuadrilateralMembraneTest
         DATA{Input/2DC0LinearQuadrilateralMembraneTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC0LinearQuadrilateralMembraneTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC0LinearTriangleMembraneTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMElement2DC0LinearTriangleMembraneTest
         DATA{Input/2DC0LinearTriangleMembraneTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC0LinearTriangleMembraneTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC0LinearTriangleStressTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMElement2DC0LinearTriangleStressTest
         DATA{Input/2DC0LinearTriangleStressTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC0LinearTriangleStressTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC0LinearTriangleStrainTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMElement2DC0LinearTriangleStrainTest
         DATA{Input/2DC0LinearTriangleStrainTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC0LinearTriangleStrainTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC0QuadraticTriangleStrainTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMElement2DC0QuadraticTriangleStrainTest
         DATA{Input/2DC0QuadraticTriangleStrainTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC0QuadraticTriangleStrainTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC0QuadraticTriangleStressTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMElement2DC0QuadraticTriangleStressTest
         DATA{Input/2DC0QuadraticTriangleStressTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC0QuadraticTriangleStressTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC0LinearLineStressTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMElement2DC0LinearLineStressTest
         DATA{Input/2DC0LinearLineStressTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC0LinearLineStressTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC0LinearQuadrilateralStrainItpackTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMElement2DC0LinearQuadrilateralStrainItpackTest
         DATA{Input/2DC0LinearQuadrilateralStrainTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC0LinearQuadrilateralStrainTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC1BeamTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMElement2DC1BeamTest
         DATA{Input/2DC1BeamTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC1BeamTestWrite.meta)

itk_add_test(NAME itkFEMElement3DC0LinearHexahedronStrainTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMElement3DC0LinearHexahedronStrainTest
         DATA{Input/3DC0LinearHexahedronStrainTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/3DC0LinearHexahedronStrainTestWrite.meta)

itk_add_test(NAME itkFEMElement3DC0LinearHexahedronMembraneTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMElement3DC0LinearHexahedronMembraneTest
         DATA{Input/3DC0LinearHexahedronMembraneTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/3DC0LinearHexahedronMembraneTestWrite.meta)

itk_add_test(NAME itkFEMElement3DC0LinearTetrahedronStrainTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMElement3DC0LinearTetrahedronStrainTest
         DATA{Input/3DC0LinearTetrahedronStrainTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/3DC0LinearTetrahedronStrainTestWrite.meta)

itk_add_test(NAME itkFEMElement3DC0LinearTetrahedronMembraneTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMElement3DC0LinearTetrahedronMembraneTest
         DATA{Input/3DC0LinearTetrahedronMembraneTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/3DC0LinearTetrahedronMembraneTestWrite.meta)

itk_add_test(NAME itkFEMLoadBCMFCTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMLoadBCMFCTest
         DATA{Input/LoadBCMFCTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/LoadBCMFCTestWrite.meta)

itk_add_test(NAME itkFEMLoadBCMFCTestUser ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMLoadBCMFCTestUser)

itk_add_test(NAME itkFEMLoadEdgeTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMLoadEdgeTest
         DATA{Input/LoadEdgeTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/LoadEdgeTestWrite.meta)

itk_add_test(NAME itkFEMLoadGravConstTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMLoadGravConstTest
         DATA{Input/LoadGravConstTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/LoadGravConstWrite.meta)

itk_add_test(NAME itkFEMLoadPointTestUser ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMLoadPointTestUser)

itk_add_test(NAME itkFEMLandmarkLoadImplementationTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMLandmarkLoadImplementationTest
         DATA{Input/quad-lm.meta}
         ${ITK_TEST_OUTPUT_DIR}/QuadLandmardkWrite.meta)

itk_add_test(NAME itkFEMObjectTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMObjectTest)

itk_add_test(NAME itkFEMObjectTest2 ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMObjectTest2)

itk_add_test(NAME itkFEMSpatialObjectTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMSpatialObjectTest
         DATA{Input/tri3-e.meta})

#add_test(NAME itkFEMSolverTest2D ${FEM_TESTS3}
#        COMMAND ITKFEMTestDriver itkFEMSolverTest2D
#        DATA{Input/2DC0LinearTriangleStressTest.meta}
#        ${ITK_TEST_OUTPUT_DIR}/2DC0LinearTriangleStressTestWrite_NewSolver.meta)

itk_add_test(NAME itkFEMSolverTest3D ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMSolverTest3D
         DATA{Input/3DC0LinearHexahedronMembraneTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/3DC0LinearHexahedronMembraneTestWrite_NewSolver.meta)

itk_add_test(NAME itkFEMRobustSolverTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkFEMRobustSolverTest)

itk_add_test(NAME itkImageToRectilinearFEMObjectFilter2DTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkImageToRectilinearFEMObjectFilter2DTest
         DATA{${ITK_DATA_ROOT}/Input/circle.png}
         20 20 7 7 64 49 3 0 0 0 10 40 20 33 20 80 3 0 0 1 9 8 10 11 12 20 19 33 37 38 46 45)

itk_add_test(NAME itkImageToRectilinearFEMObjectFilter3DTest ${FEM_TESTS3}
         COMMAND ITKFEMTestDriver itkImageToRectilinearFEMObjectFilter3DTest
         DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd}
         10 10 10 4 6 4 175 96 3 0 0 0 0 10 40 69.282 0 33 223.923 147.846 0 3 0 0 1 6 5 35 36 41 40 10 12 13 18 17 47 48 53 52 33 46 47 52 51 81 82 87 86)
