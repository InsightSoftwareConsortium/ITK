IF (TCL_TCLSH)
ADD_TEST(PrintSelf-Statistics ${TCL_TCLSH}
         ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
         ${ITK_SOURCE_DIR}/Code/Numerics/Statistics)
ENDIF (TCL_TCLSH)

INCLUDE_DIRECTORIES(
${ITK_SOURCE_DIR}/Code/Common
${ITK_SOURCE_DIR}/Code/Numerics
${ITK_SOURCE_DIR}/Code/Numerics/FEM
${ITK_SOURCE_DIR}/Code/Numerics/vxl
${ITK_SOURCE_DIR}/Code/Numerics/vxl/vnl
)

LINK_DIRECTORIES(
${ITK_BINARY_DIR}/Code/Common
${ITK_BINARY_DIR}/Code/Numerics
${ITK_BINARY_DIR}/Code/Numerics/FEM
${ITK_BINARY_DIR}/Code/Numerics/vxl
${ITK_BINARY_DIR}/Code/Numerics/vxl/vnl
)

SOURCE_FILES(FEM_SRCS
itkFEMElementTest
itkFEMLinearSystemWrapperItpackTest
itkFEMLinearSystemWrapperVNLTest
itkFEMLinearSystemWrapperDenseVNLTest
)

LINK_LIBRARIES (
VXLNumerics
ITKCommon
ITKNumerics
FEM
)

IF(UNIX)
  LINK_LIBRARIES (
  -lm
  )
ENDIF(UNIX)

IF(FEM_VISUALIZATION)
# Define the macro that includes all the required functions and include MFC support
ADD_DEFINITIONS( -DFEM_BUILD_VISUALIZATION /D "_AFXDLL" )
ENDIF(FEM_VISUALIZATION)

#ADD_TEST(itkFEMElementTestMenu itkFEMTests itkFEMElementTestMenu)

ADD_TEST(itkFEMC0HexahedralElement-NodalLoads-BCs itkFEMTests itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/hexa2.fem)
ADD_TEST(itkFEMC0HexahedralElement-NoLoads itkFEMTests itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/hexa3.fem)
ADD_TEST(itkFEMC0HexahedralElement-GravityLoad-BCs itkFEMTests itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/hexa4-grav.fem)
ADD_TEST(itkFEMC0QuadElement-NodalLoads-BCs itkFEMTests itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad2-small.fem)
ADD_TEST(itkFEMC0QuadElement-NoLoads itkFEMTests itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad4.fem)
ADD_TEST(itkFEMC0QuadElement-EdgeLoad-BCs itkFEMTests itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad_1_edge.fem)
ADD_TEST(itkFEMC0TetrahedralElement-NodalLoads-BCs itkFEMTests itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tetra2.fem)
ADD_TEST(itkFEMC0TetrahedralElement-NoLoads itkFEMTests itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tetra3.fem)
ADD_TEST(itkFEMC0QuadElementTrapezoidalGeometry-NoLoads itkFEMTests itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/trapezoid.fem)
ADD_TEST(itkFEMC0TriangularElement-NodalLoads-BCs itkFEMTests itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tri2.fem)
ADD_TEST(itkFEMC0TriangularElement-NoLoads itkFEMTests itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tri3.fem)
ADD_TEST(itkFEMTruss itkFEMTests itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/truss.fem)
ADD_TEST(itkFEMTruss-NoLoads itkFEMTests itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/truss_noload.fem)
ADD_TEST(itkFEMLinearSystemWrapperItpackTest  itkFEMTests itkFEMLinearSystemWrapperItpackTest)
ADD_TEST(itkFEMLinearSystemWrapperVNLTest  itkFEMTests itkFEMLinearSystemWrapperVNLTest)
ADD_TEST(itkFEMLinearSystemWrapperDenseVNLTest  itkFEMTests itkFEMLinearSystemWrapperDenseVNLTest)

#ADD_TEST(itkFEMBar2DTest itkFEMTests itkFEMBar2DTest)

# NOTE: the two commands below are mutually exclusive!  Do not
# uncomment both at the same time!

# To run the FEM input menu-based test locally, uncomment the line below
# ADD_EXECUTABLE(itkFEMElementTestMenu itkFEMElementTestMenu)

# To create the standard ITK test program, uncomment the line below
ADD_EXECUTABLE(itkFEMTests itkFEMTests.cxx FEM_SRCS)




