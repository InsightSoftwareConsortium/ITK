INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Wrapping/Tcl/Numerics)

ADD_LIBRARY(ITKCommonTcl)
CABLE_WRAP_TCL(ITKCommonTcl
  wrap_ITKCommon.cxx
  wrap_ITKBase.cxx
  wrap_itkImage.cxx
)

LINK_LIBRARIES(ITKCommon VXLNumerics ${CABLE_TCL_LIBRARY})
