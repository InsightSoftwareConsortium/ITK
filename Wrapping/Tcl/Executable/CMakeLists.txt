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
  ${CABLE_TCL_LIBRARY}
  ${TK_LIBRARY}
)

INSTALL_TARGETS(/lib/InsightToolkit itktcl)
