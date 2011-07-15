itk_module_test()
# Find the top of the main ITK source tree.
get_filename_component(ITK_TOP_DIR ${ITK-FEM_SOURCE_DIR}/../../.. ABSOLUTE)
include(${ITK_TOP_DIR}/CMake/ITKExternalData.cmake)

set(ITK-FEMTests
itkFEMHeaderTest.cxx
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
itkFEMLandmarkLoadImplementationTest.cxx
#itkFEMRegistrationFilterTest.cxx
# itkFEMSolverTest2D.cxx
itkFEMSolverTest3D.cxx
itkImageToRectilinearFEMObjectFilter2DTest.cxx
itkImageToRectilinearFEMObjectFilter3DTest.cxx
itkFEMElement2DTest.cxx
itkFEMElement3DTest.cxx
)

CreateTestDriver(ITK-FEM  "${ITK-FEM-Test_LIBRARIES}" "${ITK-FEMTests}")

itk_add_test(NAME itkFEMHeaderTest
      COMMAND ITK-FEMTestDriver itkFEMHeaderTest)
itk_add_test(NAME itkFEMElement2DMembraneTest
      COMMAND ITK-FEMTestDriver itkFEMElement2DMembraneTest)
itk_add_test(NAME itkFEMElement2DQuadraticTriangularTest
      COMMAND ITK-FEMTestDriver itkFEMElement2DQuadraticTriangularTest)
itk_add_test(NAME itkFEMElement2DStrainTest
      COMMAND ITK-FEMTestDriver itkFEMElement2DStrainTest)
itk_add_test(NAME itkFEMElement3DMembraneTest
      COMMAND ITK-FEMTestDriver itkFEMElement3DMembraneTest)
itk_add_test(NAME itkFEMExceptionTest
      COMMAND ITK-FEMTestDriver itkFEMExceptionTest)
itk_add_test(NAME itkFEMLinearSystemWrapperDenseVNLTest
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperDenseVNLTest)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest1
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              0)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest2
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              1)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest3
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              2)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest4
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              3)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest5
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              4)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest6
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              5)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest7
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              6)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest8
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              0)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest9
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              1)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest10
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              2)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest11
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              3)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest12
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              4)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest13
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              5)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest14
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              6)
itk_add_test(NAME itkFEMLinearSystemWrapperVNLTest
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperVNLTest)
itk_add_test(NAME itkFEMPArrayTest
      COMMAND ITK-FEMTestDriver itkFEMPArrayTest)


# 3D Element Tests
itk_add_test(NAME itkFEMC0HexahedralElement-NodalLoads-BCs
      COMMAND ITK-FEMTestDriver itkFEMElement3DTest
              DATA{Input/hexa2.meta})
itk_add_test(NAME itkFEMC0HexahedralElement-NoLoads
      COMMAND ITK-FEMTestDriver itkFEMElement3DTest
              DATA{Input/hexa3.meta})
itk_add_test(NAME itkFEMC0HexahedralElement-GravityLoad-BCs
      COMMAND ITK-FEMTestDriver itkFEMElement3DTest
              DATA{Input/hexa4-grav.meta})
itk_add_test(NAME itkFEMC0TetrahedralElement-NodalLoads-BCs
      COMMAND ITK-FEMTestDriver itkFEMElement3DTest
              DATA{Input/tetra2.meta})
itk_add_test(NAME itkFEMC0TetrahedralElement-NoLoads
      COMMAND ITK-FEMTestDriver itkFEMElement3DTest
              DATA{Input/tetra3.meta})
itk_add_test(NAME itkFEMC0TetrahedralElement-GravityLoad-BCs
      COMMAND ITK-FEMTestDriver itkFEMElement3DTest
              DATA{Input/tetra4-grav.meta})


# 2D Element Tests
itk_add_test(NAME itkFEMC0QuadElement-NodalLoads-BCs
      COMMAND ITK-FEMTestDriver itkFEMElement2DTest
              DATA{Input/quad2-small.meta})
itk_add_test(NAME itkFEMC0QuadElement-Strain
      COMMAND ITK-FEMTestDriver itkFEMElement2DTest
              DATA{Input/quad2-strain.meta})
itk_add_test(NAME itkFEMC0QuadElement-NoLoads
      COMMAND ITK-FEMTestDriver itkFEMElement2DTest
              DATA{Input/quad4.meta})
itk_add_test(NAME itkFEMC0QuadElement-GravityLoad-BCs
      COMMAND ITK-FEMTestDriver itkFEMElement2DTest
              DATA{Input/quad6-grav.meta})
itk_add_test(NAME itkFEMLoadLandmarkImplementation
      COMMAND ITK-FEMTestDriver itkFEMElement2DTest
              DATA{Input/quad-lm.meta})
itk_add_test(NAME itkFEMC0QuadElementTrapezoidalGeometry-NoLoads
      COMMAND ITK-FEMTestDriver itkFEMElement2DTest
              DATA{Input/trapezoid.meta})
itk_add_test(NAME itkFEMC0TriangularElement-NodalLoads-BCs
      COMMAND ITK-FEMTestDriver itkFEMElement2DTest
              DATA{Input/tri2.meta})
itk_add_test(NAME itkFEMC0TriangularElement-NoLoads
      COMMAND ITK-FEMTestDriver itkFEMElement2DTest
              DATA{Input/tri3.meta})
itk_add_test(NAME itkFEMC0TriangularElement-Strain
      COMMAND ITK-FEMTestDriver itkFEMElement2DTest
              DATA{Input/tri3-e.meta})
itk_add_test(NAME itkFEMC0TriangularElement-Quadratic
      COMMAND ITK-FEMTestDriver itkFEMElement2DTest
              DATA{Input/tri3-q.meta})
itk_add_test(NAME itkFEMTruss
      COMMAND ITK-FEMTestDriver itkFEMElement2DTest
              DATA{Input/truss.meta})




itk_add_test(NAME itkFEMElement2DC0LinearQuadrilateralStrainTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMElement2DC0LinearQuadrilateralStrainTest
         DATA{Input/2DC0LinearQuadrilateralStrainTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC0LinearQuadrilateralStrainTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC0LinearQuadrilateralStressTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMElement2DC0LinearQuadrilateralStressTest
         ${ITK_TEST_OUTPUT_DIR}/2DC0LinearQuadrilateralStressTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC0LinearQuadrilateralMembraneTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMElement2DC0LinearQuadrilateralMembraneTest
         DATA{Input/2DC0LinearQuadrilateralMembraneTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC0LinearQuadrilateralMembraneTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC0LinearTriangleMembraneTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMElement2DC0LinearTriangleMembraneTest
         DATA{Input/2DC0LinearTriangleMembraneTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC0LinearTriangleMembraneTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC0LinearTriangleStressTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMElement2DC0LinearTriangleStressTest
         DATA{Input/2DC0LinearTriangleStressTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC0LinearTriangleStressTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC0LinearTriangleStrainTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMElement2DC0LinearTriangleStrainTest
         DATA{Input/2DC0LinearTriangleStrainTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC0LinearTriangleStrainTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC0QuadraticTriangleStrainTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMElement2DC0QuadraticTriangleStrainTest
         DATA{Input/2DC0QuadraticTriangleStrainTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC0QuadraticTriangleStrainTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC0QuadraticTriangleStressTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMElement2DC0QuadraticTriangleStressTest
         DATA{Input/2DC0QuadraticTriangleStressTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC0QuadraticTriangleStressTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC0LinearLineStressTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMElement2DC0LinearLineStressTest
         DATA{Input/2DC0LinearLineStressTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC0LinearLineStressTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC0LinearQuadrilateralStrainItpackTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMElement2DC0LinearQuadrilateralStrainItpackTest
         DATA{Input/2DC0LinearQuadrilateralStrainTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC0LinearQuadrilateralStrainTestWrite.meta)

itk_add_test(NAME itkFEMElement2DC1BeamTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMElement2DC1BeamTest
         DATA{Input/2DC1BeamTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/2DC1BeamTestWrite.meta)

itk_add_test(NAME itkFEMElement3DC0LinearHexahedronStrainTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMElement3DC0LinearHexahedronStrainTest
         DATA{Input/3DC0LinearHexahedronStrainTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/3DC0LinearHexahedronStrainTestWrite.meta)

itk_add_test(NAME itkFEMElement3DC0LinearHexahedronMembraneTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMElement3DC0LinearHexahedronMembraneTest
         DATA{Input/3DC0LinearHexahedronMembraneTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/3DC0LinearHexahedronMembraneTestWrite.meta)

itk_add_test(NAME itkFEMElement3DC0LinearTetrahedronStrainTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMElement3DC0LinearTetrahedronStrainTest
         DATA{Input/3DC0LinearTetrahedronStrainTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/3DC0LinearTetrahedronStrainTestWrite.meta)

itk_add_test(NAME itkFEMElement3DC0LinearTetrahedronMembraneTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMElement3DC0LinearTetrahedronMembraneTest
         DATA{Input/3DC0LinearTetrahedronMembraneTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/3DC0LinearTetrahedronMembraneTestWrite.meta)

itk_add_test(NAME itkFEMLoadBCMFCTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMLoadBCMFCTest
         DATA{Input/LoadBCMFCTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/LoadBCMFCTestWrite.meta)

itk_add_test(NAME itkFEMLoadBCMFCTestUser ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMLoadBCMFCTestUser)

itk_add_test(NAME itkFEMLoadEdgeTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMLoadEdgeTest
         DATA{Input/LoadEdgeTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/LoadEdgeTestWrite.meta)

itk_add_test(NAME itkFEMLoadGravConstTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMLoadGravConstTest
         DATA{Input/LoadGravConstTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/LoadGravConstWrite.meta)

itk_add_test(NAME itkFEMLandmarkLoadImplementationTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMLandmarkLoadImplementationTest
         DATA{Input/quad-lm.meta}
         ${ITK_TEST_OUTPUT_DIR}/QuadLandmardkWrite.meta)

#add_test(NAME itkFEMRegistrationFilterTest ${FEM_TESTS3}
#         COMMAND ITK-FEMTestDriver itkFEMRegistrationFilterTest)

#add_test(NAME itkFEMSolverTest2D ${FEM_TESTS3}
#        COMMAND ITK-FEMTestDriver itkFEMSolverTest2D
#        DATA{Input/2DC0LinearTriangleStressTest.meta}
#        ${ITK_TEST_OUTPUT_DIR}/2DC0LinearTriangleStressTestWrite_NewSolver.meta)

itk_add_test(NAME itkFEMSolverTest3D ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkFEMSolverTest3D
         DATA{Input/3DC0LinearHexahedronMembraneTest.meta}
         ${ITK_TEST_OUTPUT_DIR}/3DC0LinearHexahedronMembraneTestWrite_NewSolver.meta)

itk_add_test(NAME itkImageToRectilinearFEMObjectFilter2DTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkImageToRectilinearFEMObjectFilter2DTest
         ${ITK_DATA_ROOT}/Input/circle.png
         20 20 7 7 64 49 3 0 0 0 10 14 7 33 7 28 3 0 0 1 9 8 10 11 12 20 19 33 37 38 46 45)

itk_add_test(NAME itkImageToRectilinearFEMObjectFilter3DTest ${FEM_TESTS3}
         COMMAND ITK-FEMTestDriver itkImageToRectilinearFEMObjectFilter3DTest
         ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd
         10 10 10 4 6 4 175 96 3 0 0 0 0 10 24 41.5692 0 33 122.497 100.172 0 3 0 0 1 6 5 35 36 41 40 10 12 13 18 17 47 48 53 52 33 46 47 52 51 81 82 87 86)
