# Source files for the FEM project

set(FEM_SRCS

  itkFEMElementBase.cxx

  itkFEMElement2DC0LinearLine.cxx
  itkFEMElement2DC0LinearLineStress.cxx
  itkFEMElement2DC1Beam.cxx
  itkFEMElement2DC0LinearQuadrilateral.cxx
  itkFEMElement2DC0LinearQuadrilateralMembrane.cxx
  itkFEMElement2DC0LinearQuadrilateralStress.cxx
  itkFEMElement2DC0LinearQuadrilateralStrain.cxx
  itkFEMElement2DC0LinearTriangular.cxx
  itkFEMElement2DC0LinearTriangularMembrane.cxx
  itkFEMElement2DC0LinearTriangularStress.cxx
  itkFEMElement2DC0LinearTriangularStrain.cxx
  itkFEMElement2DC0QuadraticTriangular.cxx
  itkFEMElement2DC0QuadraticTriangularStress.cxx
  itkFEMElement2DC0QuadraticTriangularStrain.cxx
  itkFEMElement3DC0LinearHexahedron.cxx
  itkFEMElement3DC0LinearHexahedronMembrane.cxx
  itkFEMElement3DC0LinearHexahedronStrain.cxx
  itkFEMElement3DC0LinearTetrahedron.cxx
  itkFEMElement3DC0LinearTetrahedronMembrane.cxx
  itkFEMElement3DC0LinearTetrahedronStrain.cxx

  itkFEMMaterialLinearElasticity.cxx

  itkFEMLoadBC.cxx
  itkFEMLoadBCMFC.cxx
  itkFEMLoadNode.cxx
  itkFEMLoadElementBase.cxx
  itkFEMLoadGrav.cxx
  itkFEMLoadEdge.cxx
  itkFEMLoadPoint.cxx
  itkFEMLoadLandmark.cxx

  itkFEMLoadImplementationsRegister.cxx
  itkFEMLoadImplementationGenericBodyLoad.cxx
  itkFEMLoadImplementationGenericLandmarkLoad.cxx

  itkFEMSolver.cxx
  itkFEMSolverHyperbolic.cxx
  itkFEMSolverCrankNicolson.cxx

  itkFEMGenerateMesh.cxx

  itkFEMLinearSystemWrapper.cxx
  itkFEMLinearSystemWrapperVNL.cxx
  itkFEMLinearSystemWrapperDenseVNL.cxx
  itkFEMLinearSystemWrapperItpack.cxx
  itkFEMItpackSparseMatrix.cxx

  itkFEMLightObject.cxx
  itkFEMException.cxx
  itkFEMUtility.cxx
  itkFEMInitialization.cxx

  dsrc2c.c
)



# Header and template source files for FEM project
# we can only include them if compiling on windows
if(NOT UNIX)
if(WIN32)
set(FEM_SRCS ${FEM_SRCS}

  itkFEMElements.h
  itkFEMElementBase.h
  itkFEMElementStd.h

  itkFEMElement1DStress.h
  itkFEMElement2DStress.h
  itkFEMElement2DMembrane.h
  itkFEMElement3DMembrane.h
  itkFEMElement3DStrain.h
  itkFEMElement2DC0LinearLine.h
  itkFEMElement2DC0LinearLineStress.h
  itkFEMElement2DC1Beam.h
  itkFEMElement2DC0LinearQuadrilateral.h
  itkFEMElement2DC0LinearQuadrilateralMembrane.h
  itkFEMElement2DC0LinearQuadrilateralStress.h
  itkFEMElement2DC0LinearTriangular.h
  itkFEMElement2DC0LinearTriangularMembrane.h
  itkFEMElement2DC0LinearTriangularStress.h
  itkFEMElement2DC0QuadraticTriangular.h
  itkFEMElement2DC0QuadraticTriangularStress.h
  itkFEMElement3DC0LinearHexahedron.h
  itkFEMElement3DC0LinearHexahedronMembrane.h
  itkFEMElement3DC0LinearHexahedronStrain.h
  itkFEMElement3DC0LinearTetrahedron.h
  itkFEMElement3DC0LinearTetrahedronMembrane.h
  itkFEMElement3DC0LinearTetrahedronStrain.h

  itkFEMElementStd.txx
  itkFEMElement1DStress.txx
  itkFEMElement2DStress.txx
  itkFEMElement2DMembrane.txx
  itkFEMElement3DMembrane.txx
  itkFEMElement3DStrain.txx

  itkFEMMaterials.h
  itkFEMMaterialBase.h
  itkFEMMaterialLinearElasticity.h

  itkFEMLoads.h
  itkFEMLoadBase.h
  itkFEMLoadBC.h
  itkFEMLoadBCMFC.h
  itkFEMLoadNode.h
  itkFEMLoadElementBase.h
  itkFEMLoadGrav.h
  itkFEMLoadEdge.h
  itkFEMLoadPoint.h
  itkFEMLoadLandmark.h
  itkFEMImageMetricLoad.h

  itkFEMLoadImplementationGenericBodyLoad.h
  itkFEMLoadImplementationGenericLandmarkLoad.h
  itkFEMLoadImplementationTest.h
  itkFEMImageMetricLoadImplementation.h

  itkFEMLoadTest.h

  itkFEMSolver.h
  itkFEMSolverHyperbolic.h
  itkFEMSolverCrankNicolson.h

  itkFEMGenerateMesh.h

  itkFEMSolution.h

  itkFEMLinearSystemWrappers.h
  itkFEMLinearSystemWrapper.h
  itkFEMLinearSystemWrapperVNL.h
  itkFEMLinearSystemWrapperDenseVNL.h
  itkFEMLinearSystemWrapperItpack.h
  itkFEMItpackSparseMatrix.h

  itkFEM.h
  itkFEMLightObject.h
  itkFEMP.h
  itkFEMPArray.h
  itkFEMObjectFactory.h
  itkFEMMacro.h
  itkFEMUtility.h
  itkFEMException.h
  itkFEMInitialization.h

  itkVisitorDispatcher.h

  itkFEMImageMetricLoad.txx

  ../../Algorithms/itkFEMRegistrationFilter.h
  ../../Algorithms/itkFEMRegistrationFilter.txx

)
endif(WIN32)
endif(NOT UNIX)

if(CMAKE_BUILD_TOOL MATCHES ".*dev")
  set(FEM_SRCS ${FEM_SRCS};ReadMe.txt;ToDo.txt)
endif(CMAKE_BUILD_TOOL MATCHES ".*dev")

# Avoid anachronism warnings by Sun compilers
include(${ITK_SOURCE_DIR}/CMake/itkCheckCXXAcceptsFlags.cmake)
itkCHECK_CXX_ACCEPTS_FLAGS("-features=no%anachronisms" SUN_COMPILER)
if(SUN_COMPILER)
  add_definitions("-w")
endif(SUN_COMPILER)

# Define source groups that look nice in DSP files
source_group("Elements" REGULAR_EXPRESSION itkFEMElement*)
source_group("Nodes" REGULAR_EXPRESSION itkFEMNode*)
source_group("Loads" REGULAR_EXPRESSION itkFEMLoad*)
source_group("Materials" REGULAR_EXPRESSION itkFEMMaterial*)
source_group("Linear Systems" REGULAR_EXPRESSION itkFEMLinearSystem*)
source_group("Source Files Template" REGULAR_EXPRESSION ./*.txx)

include_directories(${VXL_NETLIB_INCLUDE_DIR}) # Some .cxx files need f2c.h.

# Build the library
# For cygwin and mingw, there are issues with the ITKFEM shared library
# Force ITKFEM on these platforms to be static until these issues are resolved
#
if(WIN32)
  if(CMAKE_COMPILER_IS_GNUCXX)
    if(MINGW)
      add_library(ITKFEM STATIC ${FEM_SRCS})
    elseif(CYGWIN)
      add_library(ITKFEM STATIC ${FEM_SRCS})
    else(MINGW)
      add_library(ITKFEM ${FEM_SRCS})
    endif(MINGW)
  else(CMAKE_COMPILER_IS_GNUCXX)
    add_library(ITKFEM ${FEM_SRCS})
  endif(CMAKE_COMPILER_IS_GNUCXX)
else(WIN32)
  add_library(ITKFEM ${FEM_SRCS})
endif(WIN32)

target_link_libraries(ITKFEM
                      ITKBasicFilters
                      ITKIO)
if(UNIX)
  target_link_libraries(ITKFEM -lm )
endif(UNIX)

if(ITK_LIBRARY_PROPERTIES)
  set_target_properties(ITKFEM PROPERTIES ${ITK_LIBRARY_PROPERTIES})
endif(ITK_LIBRARY_PROPERTIES)

if(NOT ITK_INSTALL_NO_LIBRARIES)
  install(TARGETS ITKFEM
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
endif(NOT ITK_INSTALL_NO_LIBRARIES)

if(ITK_USE_FLAT_DIRECTORY_INSTALL)
  set( ITK_INSTALL_INCLUDE_DIR_CM24_NUMERICS_FEM ${ITK_INSTALL_INCLUDE_DIR_CM24})
else(ITK_USE_FLAT_DIRECTORY_INSTALL)
  set( ITK_INSTALL_INCLUDE_DIR_CM24_NUMERICS_FEM ${ITK_INSTALL_INCLUDE_DIR_CM24}/Numerics/FEM)
endif(ITK_USE_FLAT_DIRECTORY_INSTALL)


if(NOT ITK_INSTALL_NO_DEVELOPMENT)
  file(GLOB __files1 "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
  file(GLOB __files2 "${CMAKE_CURRENT_SOURCE_DIR}/*.txx")
  install(FILES ${__files1} ${__files2}
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24_NUMERICS_FEM}
    COMPONENT Development)
endif(NOT ITK_INSTALL_NO_DEVELOPMENT)
