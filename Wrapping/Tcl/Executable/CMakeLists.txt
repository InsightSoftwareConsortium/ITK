CONFIGURE_FILE(
  ${ITK_SOURCE_DIR}/Wrapping/Tcl/Executable/itkTclConfigure.h.in
  ${ITK_BINARY_DIR}/Wrapping/Tcl/Executable/itkTclConfigure.h
)

INCLUDE_DIRECTORIES(${ITK_BINARY_DIR}/Wrapping/Tcl/Executable)
ADD_EXECUTABLE(itktcl itkTclAppInit.cxx)
TARGET_LINK_LIBRARIES(itktcl
  VXLNumericsTcl
  ITKCommonTcl
  ITKCommon
  VXLNumerics
)
