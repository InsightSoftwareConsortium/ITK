# Source files for the FEM project
SOURCE_FILES(FEM_SRCS 

  itkFEMNodeBase.cxx
  itkFEMNodeXY.cxx
  itkFEMNodeXYrotZ.cxx
  itkFEMNodeXYZ.cxx
  itkFEMNode2DIsotropic.cxx
  
  itkFEMElementBase.cxx
  itkFEMElementBar2D.cxx
  itkFEMElementBeam2D.cxx
  itkFEMElementTriC02D.cxx
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

  itkFEMSolver.cxx
  itkFEMLinearSystemWrapperVNL.cxx

  itkFEMLightObject.cxx
  itkFEMException.cxx
  itkFEMUtility.cxx

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
  itkFEMNode2DIsotropic.h

  itkFEMElements.h
  itkFEMElementBase.h
  itkFEMElementBar2D.h
  itkFEMElementBeam2D.h
  itkFEMElementTriC02D.h
  itkFEMElementQuadC02D.h
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
 
  itkFEMSolver.h

  itkFEMLinearSystemWrapper.h
  itkFEMLinearSystemWrapperVNL.h
  
  itkFEM.h
  itkFEMLightObject.h
  itkFEMP.h
  itkFEMPArray.h
  itkFEMObjectFactory.h
  itkFEMMacro.h
  itkFEMUtility.h
  itkFEMException.h

  ReadMe.txt
  ToDo.txt

)
ENDIF(WIN32)
ENDIF(NOT UNIX)


# Define source groups that look nice in DSP files
SOURCE_GROUP("Elements" itkFEMElement*)
SOURCE_GROUP("Nodes" itkFEMNode*)
SOURCE_GROUP("Loads" itkFEMLoad*)
SOURCE_GROUP("Materials" itkFEMMaterial*)
SOURCE_GROUP("Source Files Template" ./*.txx)


# Add an option to use SmartPointers
OPTION(FEM_USE_SMART_POINTERS "Select whether Finite Elements Modeling (FEM) classes will be derived from itk::LightObject and use SmartPointers or not. If you select to use SmartPointers, be sure to define FEM_USE_SMART_POINTERS macro before including the FEM classes.")
IF(FEM_USE_SMART_POINTERS)
# Define the macro that includes smartpointers
ADD_DEFINITIONS( -DFEM_USE_SMART_POINTERS )
ENDIF(FEM_USE_SMART_POINTERS)




# Add an option to build the visualization functions
IF(WIN32)
    OPTION(FEM_VISUALIZATION "Select whether the visualization functions will be built for Finite Elements Modeling (FEM) classes or not. If you select to build visualization functions, be sure to define FEM_BUILD_VISUALIZATION macro before including the FEM classes.")
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
