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
itkFEMElementTestMenu
itkFEMElementTest
itkFEMBar2DTest
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

ADD_TEST(itkFEMElementTestMenu itkFEMTests itkFEMElementTestMenu)
ADD_TEST(itkFEMElementTest itkFEMTests itkFEMElementTest)
ADD_TEST(itkFEMBar2DTest itkFEMTests itkFEMBar2DTest)

# NOTE: the two commands below are mutually exclusive!  Do not
# uncomment both at the same time!

# To run the FEM input menu-based test locally, uncomment the line below
# ADD_EXECUTABLE(itkFEMElementTestMenu itkFEMElementTestMenu)

# To create the standard ITK test program, uncomment the line below
ADD_EXECUTABLE(itkFEMTests itkFEMTests.cxx FEM_SRCS)




