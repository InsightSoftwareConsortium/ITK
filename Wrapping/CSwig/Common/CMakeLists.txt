# create a list of cable config files for wrapping
SET(WRAP_SOURCES
  wrap_ITKBase
  wrap_ITKFilterBase
  wrap_ITKInterpolators
  wrap_ITKKernelDeformableTransforms
  wrap_ITKRegions
  wrap_ITKRigidTransforms
  wrap_ITKSimilarityTransforms
  wrap_itkArray
  wrap_itkAffineTransform
  wrap_itkAzimuthElevationToCartesianTransform
  wrap_itkBSplineDeformableTransform
  wrap_itkContinuousIndex
  wrap_itkDenseFiniteDifferenceImageFilter
  wrap_itkDifferenceImageFilter
  wrap_itkEventObject
  wrap_itkFiniteDifferenceImageFilter
  wrap_itkFixedArray
  wrap_itkIdentityTransform
  wrap_itkImage
  wrap_itkIndex
  wrap_itkPoint
  wrap_itkSize
  wrap_itkScaleTransform
  wrap_itkTransform
  wrap_itkTranslationTransform
  wrap_itkVector
  wrap_itkVersorTransform
)

SET(INDEX_FILE_CONTENT "")

# loop over cable config files creating two lists:
# WRAP_TCL_SOURCES: list of generated files
FOREACH(Source ${WRAP_SOURCES})
   SET(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} ${Source}Tcl.cxx)
   SET(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} ${Source}Python.cxx)
   SET(WRAP_JAVA_SOURCES ${WRAP_JAVA_SOURCES} ${Source}Java.cxx)
   SET(ALL_IDX_FILES ${ALL_IDX_FILES} ${WrapITK_BINARY_DIR}/Common/${Source}.idx )
   SET(INDEX_FILE_CONTENT "${INDEX_FILE_CONTENT}${WrapITK_BINARY_DIR}/Common/${Source}.idx\n")
ENDFOREACH(Source)
SET(WRAP_TCL_SOURCES ${WRAP_TCL_SOURCES} wrap_ITKCommonTclTcl.cxx wrap_ITKUtilsTcl.cxx)
SET(WRAP_PYTHON_SOURCES ${WRAP_PYTHON_SOURCES} wrap_ITKCommonPythonPython.cxx wrap_ITKPyUtilsPython.cxx)

CONFIGURE_FILE(
${WrapITK_SOURCE_DIR}/Master.mdx.in
${WrapITK_BINARY_DIR}/Common/ITKCommonTcl.mdx
)

# set the generated sources as generated
SET_SOURCE_FILES_PROPERTIES(${WRAP_TCL_SOURCES}
                            ${WRAP_PYTHON_SOURCES}
                            ${WRAP_JAVA_SOURCES} GENERATED )


IF(ITK_CSWIG_TCL)
  SET(SWIG_INC ${SWIG_INC} -I${TCL_INCLUDE_PATH})
  # add the library
  SET_SOURCE_FILES_PROPERTIES(SwigExtras_wrapTcl.cxx GENERATED)
  ADD_LIBRARY(ITKCommonTcl SHARED ${WRAP_TCL_SOURCES} itkStringStream.cxx itkTclCommand.cxx SwigExtras_wrapTcl.cxx)
  ADD_CUSTOM_COMMAND(
    COMMENT "run native swig on SwigExtras.i"
    SOURCE ${ITK_TOP}/Wrapping/CSwig/Common/SwigExtras.i
    COMMAND ${CSWIG} 
    ARGS -nocable -c ${IGNORE_WARNINGS} -o ${WrapITK_BINARY_DIR}/Common/SwigExtras_wrapTcl.cxx
         -c++ ${ITK_TOP}/Wrapping/CSwig/Common/SwigExtras.i
    TARGET ITKCommonTcl
    OUTPUTS ${WrapITK_BINARY_DIR}/Common/SwigExtras_wrapTcl.cxx
    DEPENDS ${ITK_TOP}/Wrapping/CSwig/Common/SwigExtras.i)
  TARGET_LINK_LIBRARIES(ITKCommonTcl ${TCL_LIBRARY} ITKCommon VXLNumericsTcl SwigRuntimeTcl )
ENDIF(ITK_CSWIG_TCL)


IF(ITK_CSWIG_PYTHON)
  SET(SWIG_INC ${SWIG_INC} -I${PYTHON_INCLUDE_PATH})
  SET_SOURCE_FILES_PROPERTIES(SwigExtras_wrapPython.cxx GENERATED)
  ADD_LIBRARY(_ITKCommonPython MODULE ${WRAP_PYTHON_SOURCES} itkStringStream.cxx itkPyCommand.cxx SwigExtras_wrapPython.cxx )
  ADD_CUSTOM_COMMAND(
    COMMENT "run native swig on SwigExtras.i"
    SOURCE ${ITK_TOP}/Wrapping/CSwig/Common/SwigExtras.i
    COMMAND ${CSWIG} 
    ARGS -nocable -c ${IGNORE_WARNINGS} 
         -o ${WrapITK_BINARY_DIR}/Common/SwigExtras_wrapPython.cxx
         -bindir "${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}"
         -python -c++ ${ITK_TOP}/Wrapping/CSwig/Common/SwigExtras.i
    TARGET _ITKCommonPython
    OUTPUTS ${WrapITK_BINARY_DIR}/Common/SwigExtras_wrapPython.cxx
    DEPENDS ${ITK_TOP}/Wrapping/CSwig/Common/SwigExtras.i)
  SET_TARGET_PROPERTIES( _ITKCommonPython PROPERTIES PREFIX "")
  # link to ITKCommon
  IF(ITK_PYTHON_NEEDS_DEPENDS)
    ADD_DEPENDENCIES(_ITKCommonPython _VXLNumericsPython)
  ENDIF(ITK_PYTHON_NEEDS_DEPENDS)
  TARGET_LINK_LIBRARIES(_ITKCommonPython ${PYTHON_LIBRARY} ITKCommon SwigRuntimePython)
ENDIF(ITK_CSWIG_PYTHON)

IF(ITK_CSWIG_JAVA) 
  SET_SOURCE_FILES_PROPERTIES(SwigExtras_wrapJava.cxx GENERATED)
  ADD_LIBRARY(ITKCommonJava MODULE ${WRAP_JAVA_SOURCES} SwigExtras_wrapJava.cxx )
  ADD_CUSTOM_COMMAND(
    COMMENT "run native swig on SwigExtras.i"
    SOURCE ${ITK_TOP}/Wrapping/CSwig/Common/SwigExtras.i
    COMMAND ${CSWIG} 
    ARGS -nocable -c ${IGNORE_WARNINGS} 
         -o ${WrapITK_BINARY_DIR}/Common/SwigExtras_wrapJava.cxx
         -bindir "${EXECUTABLE_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}"
         -java -c++ ${ITK_TOP}/Wrapping/CSwig/Common/SwigExtras.i
    TARGET ITKCommonJava
    OUTPUTS ${WrapITK_BINARY_DIR}/Common/SwigExtras_wrapJava.cxx
    DEPENDS ${ITK_TOP}/Wrapping/CSwig/Common/SwigExtras.i)
  # link to ITKCommon
  TARGET_LINK_LIBRARIES(ITKCommonJava ${JAVA_LIBRARY} ITKCommon SwigRuntimeJava)
ENDIF(ITK_CSWIG_JAVA)


SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumericsTcl.mdx"
                       "${WrapITK_BINARY_DIR}/Common/ITKCommonTcl.mdx"
)

# wrap each of the files
FOREACH(Source ${WRAP_SOURCES})
  IF(ITK_CSWIG_TCL)
  # tcl
    WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/Common ${WrapITK_BINARY_DIR}/Common
                     ${Source} ITKCommonTcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
  ENDIF(ITK_CSWIG_TCL)
   
  IF(ITK_CSWIG_PYTHON)
  # python
    WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/Common ${WrapITK_BINARY_DIR}/Common
                        ${Source} _ITKCommonPython "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
  ENDIF(ITK_CSWIG_PYTHON)

  IF(ITK_CSWIG_JAVA)
  # python
    WRAP_JAVA_SOURCES(${ITK_TOP}/Wrapping/CSwig/Common ${WrapITK_BINARY_DIR}/Common
                        ${Source} ITKCommonJava "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
  ENDIF(ITK_CSWIG_JAVA)
ENDFOREACH(Source)

IF(ITK_CSWIG_TCL)
# tcl
  WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/Common ${WrapITK_BINARY_DIR}/Common
                   wrap_ITKCommonTcl ITKCommonTcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}") 
  WRAP_TCL_SOURCES(${ITK_TOP}/Wrapping/CSwig/Common ${WrapITK_BINARY_DIR}/Common
                   wrap_ITKUtils ITKCommonTcl "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
ENDIF(ITK_CSWIG_TCL)
   
IF(ITK_CSWIG_PYTHON)
# python
  WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/Common ${WrapITK_BINARY_DIR}/Common
                      wrap_ITKCommonPython _ITKCommonPython "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
  WRAP_PYTHON_SOURCES(${ITK_TOP}/Wrapping/CSwig/Common ${WrapITK_BINARY_DIR}/Common
                      wrap_ITKPyUtils _ITKCommonPython "${MASTER_INDEX_FILES}" "${ALL_IDX_FILES}")
ENDIF(ITK_CSWIG_PYTHON)

