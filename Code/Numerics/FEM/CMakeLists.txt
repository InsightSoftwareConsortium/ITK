# Source files for the FEM project

INCLUDE_DIRECTORIES(
${ITK_SOURCE_DIR}/Code/Common
${ITK_SOURCE_DIR}/Code/BasicFilters
${ITK_SOURCE_DIR}/Code/Algorithms
${ITK_SOURCE_DIR}/Code/IO
${ITK_SOURCE_DIR}/Code/Numerics
${ITK_SOURCE_DIR}/Code/Numerics/vxl
${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl
)



SOURCE_FILES(FEM_SRCS 

  itkFEMNodeBase.cxx
  itkFEMNodeXY.cxx
  itkFEMNodeXYrotZ.cxx
  itkFEMNodeXYZ.cxx
  
  itkFEMElementBase.cxx
  itkFEMElementBar2D.cxx
  itkFEMElementBeam2D.cxx
  itkFEMElementTriC02D.cxx
  itkFEMElementMembraneC02D.cxx
  itkFEMElementQuadC02D.cxx
  itkFEMElementTetrahedronC03D.cxx
  itkFEMElementHexahedronC03D.cxx
  itkFEMElementC1IsoCurve2D.cxx

  itkFEMMaterialBase.cxx
  itkFEMMaterialStandard.cxx

  itkFEMLoadBase.cxx
  itkFEMLoadBCMFC.cxx
  itkFEMLoadNode.cxx
  itkFEMLoadElementBase.cxx
  itkFEMLoadGrav.cxx
  itkFEMLoadEdge.cxx
  itkFEMLoadPoint.cxx
  itkFEMLoadImagePairBase.txx
  itkFEMLoadImageMetric.txx
 
  itkFEMLoadImplementationsRegister.cxx
  itkFEMLoadImplementationGravBar2D.cxx
  itkFEMLoadImplementationGravBeam2D.cxx
  itkFEMLoadImplementationGravMembraneC02D.cxx
  itkFEMLoadImplementationGravQuadC02D.cxx
  itkFEMLoadImplementationGravTriC02D.cxx

  itkFEMLoadImplementationEdgeTriC02D.cxx
  itkFEMLoadImplementationEdgeQuadC02D.cxx

  itkFEMLoadImplementationPointBar2D.cxx
  itkFEMLoadImplementationPointBeam2D.cxx

  itkFEMLoadImplementationC1IsoCurve2D.cxx


  itkFEMSolver.cxx
  itkFEMLinearSystemWrapperVNL.cxx
  itkFEMItpackLinearSystemWrapper.cxx
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
SOURCE_FILES(FEM_SRCS 

  itkFEMNodes.h
  itkFEMNodeBase.h
  itkFEMNodeXY.h
  itkFEMNodeXYZ.h
  itkFEMNodeXYrotZ.h

  itkFEMElements.h
  itkFEMElementBase.h
  itkFEMElementStandard.h
  itkFEMElementBar2D.h
  itkFEMElementBeam2D.h
  itkFEMElementTriC02D.h
  itkFEMElementQuadC02D.h
  itkFEMElementMembraneC02D.h
  itkFEMElementTetrahedronC03D.h
  itkFEMElementHexahedronC03D.h
  itkFEMElementC1IsoCurve2D.h

  itkFEMMaterials.h
  itkFEMMaterialBase.h
  itkFEMMaterialStandard.h

  itkFEMLoads.h
  itkFEMLoadBase.h
  itkFEMLoadBCMFC.h
  itkFEMLoadNode.h
  itkFEMLoadElementBase.h
  itkFEMLoadGrav.h
  itkFEMLoadEdge.h
  itkFEMLoadPoint.h
  itkFEMLoadImagePairBase.h
  itkFEMLoadImageMetric.h

  itkFEMSolver.h

  itkFEMSolution.h

  itkFEMLinearSystemWrapper.h
  itkFEMLinearSystemWrapperVNL.h
  itkFEMItpackLinearSystemWrapper.h
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

  ReadMe.txt
  ToDo.txt

)
ENDIF(WIN32)
ENDIF(NOT UNIX)


LINK_LIBRARIES (
VXLNumerics
ITKNumerics
ITKCommon
ITKBasicFilters
ITKIO
)


# Define source groups that look nice in DSP files
SOURCE_GROUP("Elements" itkFEMElement*)
SOURCE_GROUP("Nodes" itkFEMNode*)
SOURCE_GROUP("Loads" itkFEMLoad*)
SOURCE_GROUP("Materials" itkFEMMaterial*)
SOURCE_GROUP("Source Files Template" ./*.txx)


# Add an option to use SmartPointers
OPTION(FEM_USE_SMART_POINTERS "Select whether Finite Elements Modeling (FEM) classes will be derived from itk::LightObject and use SmartPointers or not. If you select to use SmartPointers, be sure to define FEM_USE_SMART_POINTERS macro before including the FEM classes.")
MARK_AS_ADVANCED( FEM_USE_SMART_POINTERS )
IF(FEM_USE_SMART_POINTERS)
# Define the macro that includes smartpointers
ADD_DEFINITIONS( -DFEM_USE_SMART_POINTERS )
ENDIF(FEM_USE_SMART_POINTERS)




# Add an option to build the visualization functions
IF(WIN32)
    OPTION(FEM_VISUALIZATION "Select whether the visualization functions will be built for Finite Elements Modeling (FEM) classes or not. If you select to build visualization functions, be sure to define FEM_BUILD_VISUALIZATION macro before including the FEM classes.")
    MARK_AS_ADVANCED( FEM_VISUALIZATION )
    IF(FEM_VISUALIZATION)
    # Define the macro that includes all the required functions and include MFC support
    ADD_DEFINITIONS( -DFEM_BUILD_VISUALIZATION /D "_AFXDLL" )
    ENDIF(FEM_VISUALIZATION)
ENDIF(WIN32)




# Include the ITPACK library within FEM
SOURCE_FILES(FEM_SRCS
  itpack.cxx
)
IF(NOT UNIX)
IF(WIN32)
SOURCE_FILES(FEM_SRCS
  itpack.h
  itpack_f2c.h
)
ENDIF(WIN32)
ENDIF(NOT UNIX)
SOURCE_GROUP("ITPACK" itpack*)



# Build the library
ADD_LIBRARY(FEM FEM_SRCS)
