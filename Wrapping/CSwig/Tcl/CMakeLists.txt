SET(ITK_TCL_EXE_DIR_BUILD "${ITK_EXECUTABLE_PATH}")
SET(ITK_TCL_EXE_NAME_ROOT "itkwish")

CONFIGURE_FILE(${ITK_SOURCE_DIR}/Wrapping/CSwig/Tcl/itkTclConfigure.h.in
               ${ITK_BINARY_DIR}/Wrapping/CSwig/Tcl/itkTclConfigure.h)

INCLUDE_DIRECTORIES(${ITK_BINARY_DIR}/Wrapping/CSwig/Tcl)
ADD_EXECUTABLE(itkwish itkTclAppInit.cxx)
TARGET_LINK_LIBRARIES(itkwish
  ITKAlgorithmsTcl
  ITKBasicFiltersTcl
  ITKIOTcl
  ITKNumericsTcl
  ITKCommonTcl
  VXLNumericsTcl
  SwigRuntimeTcl
  ${TCL_LIBRARY}
  ${TK_LIBRARY}
)

INSTALL_TARGETS(/lib/InsightToolkit itktcl)