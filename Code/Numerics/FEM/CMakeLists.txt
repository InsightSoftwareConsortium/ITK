# Source files for the FEM project

SET(FEM_SRCS 

  itkFEMElementBase.cxx

  itkFEMElement2DC0LinearLine.cxx
  itkFEMElement2DC0LinearLineStress.cxx
  itkFEMElement2DC1Beam.cxx
  itkFEMElement2DC0LinearQuadrilateral.cxx
  itkFEMElement2DC0LinearQuadrilateralMembrane.cxx
  itkFEMElement2DC0LinearQuadrilateralStress.cxx
  itkFEMElement2DC0LinearTriangular.cxx
  itkFEMElement2DC0LinearTriangularMembrane.cxx
  itkFEMElement2DC0LinearTriangularStress.cxx
  itkFEMElement2DC0QuadraticTriangular.cxx
  itkFEMElement2DC0QuadraticTriangularStress.cxx
  itkFEMElement3DC0LinearHexahedron.cxx
  itkFEMElement3DC0LinearHexahedronMembrane.cxx
  itkFEMElement3DC0LinearHexahedronStrain.cxx
  itkFEMElement3DC0LinearTetrahedron.cxx
  itkFEMElement3DC0LinearTetrahedronMembrane.cxx
  itkFEMElement3DC0LinearTetrahedronStrain.cxx

  itkFEMMaterialBase.cxx
  itkFEMMaterialLinearElasticity.cxx

  itkFEMLoadBase.cxx
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

)



# Header and template source files for FEM project
# we can only include them if compiling on windows
IF(NOT UNIX)
IF(WIN32)
SET(FEM_SRCS ${FEM_SRCS}

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
ENDIF(WIN32)
ENDIF(NOT UNIX)

IF(CMAKE_BUILD_TOOL MATCHES ".*dev")
  SET(FEM_SRCS ${FEM_SRCS};ReadMe.txt;ToDo.txt)
ENDIF(CMAKE_BUILD_TOOL MATCHES ".*dev")

  
# Define source groups that look nice in DSP files
SOURCE_GROUP("Elements" itkFEMElement*)
SOURCE_GROUP("Nodes" itkFEMNode*)
SOURCE_GROUP("Loads" itkFEMLoad*)
SOURCE_GROUP("Materials" itkFEMMaterial*)
SOURCE_GROUP("Linear Systems" itkFEMLinearSystem*)
SOURCE_GROUP("Source Files Template" ./*.txx)


# Include the ITPACK library within FEM
SET(FEM_SRCS ${FEM_SRCS}
  itpack.cxx
)
IF(NOT UNIX)
IF(WIN32)
SET(FEM_SRCS ${FEM_SRCS}
  itpack.h
  itpack_f2c.h
)
ENDIF(WIN32)
ENDIF(NOT UNIX)
SOURCE_GROUP("ITPACK" itpack*)



# Build the library
ADD_LIBRARY(ITKFEM ${FEM_SRCS})
TARGET_LINK_LIBRARIES(ITKFEM VXLNumerics
                      ITKNumerics
                      ITKCommon
                      ITKBasicFilters
                      ITKIO)
IF(UNIX)
  TARGET_LINK_LIBRARIES (ITKFEM -lm )
ENDIF(UNIX)

INSTALL_TARGETS(/lib/InsightToolkit ITKFEM)
INSTALL_FILES(/include/InsightToolkit/Numerics/FEM "(\\.h|\\.txx)$")
