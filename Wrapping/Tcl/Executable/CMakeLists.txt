CONFIGURE_FILE(
  ${ITK_SOURCE_DIR}/Wrapping/Tcl/Executable/itkTclConfigure.h.in
  ${ITK_BINARY_DIR}/Wrapping/Tcl/Executable/itkTclConfigure.h
)

INCLUDE_DIRECTORIES(${ITK_BINARY_DIR}/Wrapping/Tcl/Executable)
ADD_EXECUTABLE(itktcl itkTclAppInit.cxx)
TARGET_LINK_LIBRARIES(itktcl
  ITKAlgorithmsTcl
  ITKBasicFiltersTcl
  ITKIOTcl
  ITKNumericsTcl
  ITKCommonTcl
  VXLNumericsTcl
  ITKIO
  ITKBasicFilters
  ITKNumerics
  ITKCommon
  VXLNumerics
  itkpng
  itkzlib
  ${CABLE_TCL_LIBRARY}
  ${TCL_LIBRARY}
  ${TK_LIBRARY}
)
