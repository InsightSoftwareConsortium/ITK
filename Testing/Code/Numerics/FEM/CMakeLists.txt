if( NOT ITK_DISABLE_CXX_TESTING )
if(ITK_BINARY_DIR)
  if(TCL_TCLSH)
    add_test(PrintSelf-FEM ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${ITK_SOURCE_DIR}/Code/Numerics/FEM)
  endif(TCL_TCLSH)
endif(ITK_BINARY_DIR)



set(FEM_TESTS ${CXX_TEST_PATH}/itkFEMTests)
set(FEM_TESTS2 ${CXX_TEST_PATH}/itkFEMTests2)

set(FEM_SRCS
itkFEMElementTest.cxx
itkFEMExceptionTest.cxx
itkFEMGenerateMeshTest.cxx
itkFEMElement2DMembraneTest.cxx
itkFEMElement3DMembraneTest.cxx
itkFEMElement2DStrainTest.cxx
)
set(FEM_SRCS2
itkFEMElement2DQuadraticTriangularTest.cxx
itkFEMLinearSystemWrapperItpackTest.cxx
itkFEMLinearSystemWrapperItpackTest2.cxx
itkFEMLinearSystemWrapperVNLTest.cxx
itkFEMLinearSystemWrapperDenseVNLTest.cxx
itkFEMPArrayTest.cxx
)

add_test(itkFEMC0HexahedralElement-NodalLoads-BCs ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/hexa2.fem)
add_test(itkFEMC0HexahedralElement-NoLoads ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/hexa3.fem)
add_test(itkFEMC0HexahedralElement-GravityLoad-BCs ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/hexa4-grav.fem)
add_test(itkFEMC0QuadElement-NodalLoads-BCs ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad2-small.fem)
add_test(itkFEMC0QuadElement-Strain ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad2-strain.fem)
add_test(itkFEMC0QuadElement-NoLoads ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad4.fem)
add_test(itkFEMC0QuadElement-GravityLoad-BCs ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad6-grav.fem)
add_test(itkFEMLoadLandmarkImplementation ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad-lm.fem)

# Edge loads are not re-implemented yet
#add_test(itkFEMC0QuadElement-EdgeLoad-BCs ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad_1_edge.fem)

add_test(itkFEMC0TetrahedralElement-NodalLoads-BCs ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tetra2.fem)
add_test(itkFEMC0TetrahedralElement-NoLoads ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tetra3.fem)
add_test(itkFEMC0TetrahedralElement-GravityLoad-BCs ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tetra4-grav.fem)
add_test(itkFEMC0QuadElementTrapezoidalGeometry-NoLoads ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/trapezoid.fem)
add_test(itkFEMC0TriangularElement-NodalLoads-BCs ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tri2.fem)
add_test(itkFEMC0TriangularElement-NoLoads ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tri3.fem)
add_test(itkFEMC0TriangularElement-Strain ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tri3-e.fem)
add_test(itkFEMC0TriangularElement-Quadratic ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tri3-q.fem)
add_test(itkFEMTruss ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/truss.fem)

add_test(itkFEMLinearSystemWrapperItpackTest  ${FEM_TESTS2} itkFEMLinearSystemWrapperItpackTest)

add_test(itkFEMLinearSystemWrapperItpackTest1  ${FEM_TESTS2} itkFEMLinearSystemWrapperItpackTest 0 )
add_test(itkFEMLinearSystemWrapperItpackTest2  ${FEM_TESTS2} itkFEMLinearSystemWrapperItpackTest 1 )
add_test(itkFEMLinearSystemWrapperItpackTest3  ${FEM_TESTS2} itkFEMLinearSystemWrapperItpackTest 2 )
add_test(itkFEMLinearSystemWrapperItpackTest4  ${FEM_TESTS2} itkFEMLinearSystemWrapperItpackTest 3 )
add_test(itkFEMLinearSystemWrapperItpackTest5  ${FEM_TESTS2} itkFEMLinearSystemWrapperItpackTest 4 )
add_test(itkFEMLinearSystemWrapperItpackTest6  ${FEM_TESTS2} itkFEMLinearSystemWrapperItpackTest 5 )
add_test(itkFEMLinearSystemWrapperItpackTest7  ${FEM_TESTS2} itkFEMLinearSystemWrapperItpackTest 6 )

add_test(itkFEMLinearSystemWrapperItpackTest8   ${FEM_TESTS2} itkFEMLinearSystemWrapperItpackTest2 0 )
add_test(itkFEMLinearSystemWrapperItpackTest9   ${FEM_TESTS2} itkFEMLinearSystemWrapperItpackTest2 1 )
add_test(itkFEMLinearSystemWrapperItpackTest10  ${FEM_TESTS2} itkFEMLinearSystemWrapperItpackTest2 2 )
add_test(itkFEMLinearSystemWrapperItpackTest11  ${FEM_TESTS2} itkFEMLinearSystemWrapperItpackTest2 3 )
add_test(itkFEMLinearSystemWrapperItpackTest12  ${FEM_TESTS2} itkFEMLinearSystemWrapperItpackTest2 4 )
add_test(itkFEMLinearSystemWrapperItpackTest13  ${FEM_TESTS2} itkFEMLinearSystemWrapperItpackTest2 5 )
add_test(itkFEMLinearSystemWrapperItpackTest14  ${FEM_TESTS2} itkFEMLinearSystemWrapperItpackTest2 6 )

add_test(itkFEMLinearSystemWrapperVNLTest  ${FEM_TESTS2} itkFEMLinearSystemWrapperVNLTest)
add_test(itkFEMLinearSystemWrapperDenseVNLTest  ${FEM_TESTS2} itkFEMLinearSystemWrapperDenseVNLTest)
add_test(itkFEMPArrayTest  ${FEM_TESTS2} itkFEMPArrayTest)

add_test(itkFEMExceptionTest ${FEM_TESTS} itkFEMExceptionTest)
add_test(itkFEMGenerateMeshTest ${FEM_TESTS} itkFEMGenerateMeshTest)
add_test(itkFEMElement2DMembraneTest ${FEM_TESTS} itkFEMElement2DMembraneTest)
add_test(itkFEMElement3DMembraneTest ${FEM_TESTS} itkFEMElement3DMembraneTest)
add_test(itkFEMElement2DStrainTest ${FEM_TESTS} itkFEMElement2DStrainTest)
add_test(itkFEMElement2DQuadraticTriangularTest ${FEM_TESTS2} itkFEMElement2DQuadraticTriangularTest)

#add_test(itkFEMBar2DTest ${FEM_TESTS} itkFEMBar2DTest)

# NOTE: the two commands below are mutually exclusive!  Do not
# uncomment both at the same time!

# To run the FEM input menu-based test locally, uncomment the line below
#add_executable(itkFEMElementTestMenu itkFEMElementTest.cxx)
#target_link_libraries(itkFEMElementTestMenu ITKNumerics ITKFEM)
#add_executable(itkFEMExceptionTest itkFEMExceptionTest.cxx)
#target_link_libraries(itkFEMExceptionTest ITKFEM)
#add_executable(itkFEMGenerateMeshTest itkFEMGenerateMeshTest.cxx)
#target_link_libraries(itkFEMGenerateMeshTest ITKNumerics ITKFEM)
#add_executable(itkFEMElement2DQuadraticTriangularTest itkFEMElement2DQuadraticTriangularTest.cxx)
#target_link_libraries(itkFEMElement2DQuadraticTriangularTest ITKNumerics ITKFEM)

# To create the standard ITK test program, uncomment the line below
add_executable(itkFEMTests itkFEMTests.cxx ${FEM_SRCS})
if(UNIX)
  target_link_libraries(itkFEMTests -lm )
endif(UNIX)
target_link_libraries(itkFEMTests ITKNumerics ITKFEM)

add_executable(itkFEMTests2 itkFEMTests2.cxx ${FEM_SRCS2})
if(UNIX)
  target_link_libraries(itkFEMTests2 -lm )
endif(UNIX)
target_link_libraries(itkFEMTests2 ITKNumerics ITKFEM)




endif( NOT ITK_DISABLE_CXX_TESTING )



