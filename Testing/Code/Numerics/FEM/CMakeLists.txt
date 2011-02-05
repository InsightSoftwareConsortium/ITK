if( NOT ITK_DISABLE_CXX_TESTING )
if(ITK_BINARY_DIR)
  if(TCL_TCLSH)
    add_test(PrintSelf-FEM ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${ITK_SOURCE_DIR}/Code/Numerics/FEM)
  endif(TCL_TCLSH)
endif(ITK_BINARY_DIR)



set(KIT FEM)
set(KIT_LIBS ITKNumerics ITKFEM)
set(KitTests
  itkFEMElement2DMembraneTest
  itkFEMElement2DQuadraticTriangularTest
  itkFEMElement2DStrainTest
  itkFEMElement3DMembraneTest
  itkFEMElementTest
  itkFEMExceptionTest
  itkFEMGenerateMeshTest
  itkFEMLinearSystemWrapperDenseVNLTest
  itkFEMLinearSystemWrapperItpackTest
  itkFEMLinearSystemWrapperItpackTest2
  itkFEMLinearSystemWrapperVNLTest
  itkFEMPArrayTest
)

# Given KIT, KIT_LIBS and KitTests, build TestDriver
CreateTestDriver(${KIT} "${KIT_LIBS}" "${KitTests}" TestDriver)

add_test(itkFEMC0HexahedralElement-NodalLoads-BCs ${TestDriver} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/hexa2.fem)
add_test(itkFEMC0HexahedralElement-NoLoads ${TestDriver} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/hexa3.fem)
add_test(itkFEMC0HexahedralElement-GravityLoad-BCs ${TestDriver} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/hexa4-grav.fem)
add_test(itkFEMC0QuadElement-NodalLoads-BCs ${TestDriver} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad2-small.fem)
add_test(itkFEMC0QuadElement-Strain ${TestDriver} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad2-strain.fem)
add_test(itkFEMC0QuadElement-NoLoads ${TestDriver} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad4.fem)
add_test(itkFEMC0QuadElement-GravityLoad-BCs ${TestDriver} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad6-grav.fem)
add_test(itkFEMLoadLandmarkImplementation ${TestDriver} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad-lm.fem)

# Edge loads are not re-implemented yet
#add_test(itkFEMC0QuadElement-EdgeLoad-BCs ${TestDriver} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad_1_edge.fem)

add_test(itkFEMC0TetrahedralElement-NodalLoads-BCs ${TestDriver} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tetra2.fem)
add_test(itkFEMC0TetrahedralElement-NoLoads ${TestDriver} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tetra3.fem)
add_test(itkFEMC0TetrahedralElement-GravityLoad-BCs ${TestDriver} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tetra4-grav.fem)
add_test(itkFEMC0QuadElementTrapezoidalGeometry-NoLoads ${TestDriver} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/trapezoid.fem)
add_test(itkFEMC0TriangularElement-NodalLoads-BCs ${TestDriver} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tri2.fem)
add_test(itkFEMC0TriangularElement-NoLoads ${TestDriver} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tri3.fem)
add_test(itkFEMC0TriangularElement-Strain ${TestDriver} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tri3-e.fem)
add_test(itkFEMC0TriangularElement-Quadratic ${TestDriver} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tri3-q.fem)
add_test(itkFEMTruss ${TestDriver} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/truss.fem)

add_test(itkFEMLinearSystemWrapperItpackTest  ${TestDriver} itkFEMLinearSystemWrapperItpackTest)

add_test(itkFEMLinearSystemWrapperItpackTest1  ${TestDriver} itkFEMLinearSystemWrapperItpackTest 0 )
add_test(itkFEMLinearSystemWrapperItpackTest2  ${TestDriver} itkFEMLinearSystemWrapperItpackTest 1 )
add_test(itkFEMLinearSystemWrapperItpackTest3  ${TestDriver} itkFEMLinearSystemWrapperItpackTest 2 )
add_test(itkFEMLinearSystemWrapperItpackTest4  ${TestDriver} itkFEMLinearSystemWrapperItpackTest 3 )
add_test(itkFEMLinearSystemWrapperItpackTest5  ${TestDriver} itkFEMLinearSystemWrapperItpackTest 4 )
add_test(itkFEMLinearSystemWrapperItpackTest6  ${TestDriver} itkFEMLinearSystemWrapperItpackTest 5 )
add_test(itkFEMLinearSystemWrapperItpackTest7  ${TestDriver} itkFEMLinearSystemWrapperItpackTest 6 )

add_test(itkFEMLinearSystemWrapperItpackTest8   ${TestDriver} itkFEMLinearSystemWrapperItpackTest2 0 )
add_test(itkFEMLinearSystemWrapperItpackTest9   ${TestDriver} itkFEMLinearSystemWrapperItpackTest2 1 )
add_test(itkFEMLinearSystemWrapperItpackTest10  ${TestDriver} itkFEMLinearSystemWrapperItpackTest2 2 )
add_test(itkFEMLinearSystemWrapperItpackTest11  ${TestDriver} itkFEMLinearSystemWrapperItpackTest2 3 )
add_test(itkFEMLinearSystemWrapperItpackTest12  ${TestDriver} itkFEMLinearSystemWrapperItpackTest2 4 )
add_test(itkFEMLinearSystemWrapperItpackTest13  ${TestDriver} itkFEMLinearSystemWrapperItpackTest2 5 )
add_test(itkFEMLinearSystemWrapperItpackTest14  ${TestDriver} itkFEMLinearSystemWrapperItpackTest2 6 )

add_test(itkFEMLinearSystemWrapperVNLTest  ${TestDriver} itkFEMLinearSystemWrapperVNLTest)
add_test(itkFEMLinearSystemWrapperDenseVNLTest  ${TestDriver} itkFEMLinearSystemWrapperDenseVNLTest)
add_test(itkFEMPArrayTest  ${TestDriver} itkFEMPArrayTest)

add_test(itkFEMExceptionTest ${TestDriver} itkFEMExceptionTest)
add_test(itkFEMGenerateMeshTest ${TestDriver} itkFEMGenerateMeshTest)
add_test(itkFEMElement2DMembraneTest ${TestDriver} itkFEMElement2DMembraneTest)
add_test(itkFEMElement3DMembraneTest ${TestDriver} itkFEMElement3DMembraneTest)
add_test(itkFEMElement2DStrainTest ${TestDriver} itkFEMElement2DStrainTest)
add_test(itkFEMElement2DQuadraticTriangularTest ${TestDriver} itkFEMElement2DQuadraticTriangularTest)

#add_test(itkFEMBar2DTest ${TestDriver} itkFEMBar2DTest)

endif( NOT ITK_DISABLE_CXX_TESTING )



