INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Wrapping/Tcl/Common)

ADD_LIBRARY(ITKCommonTcl
  itkTclCommand.cxx
  itkTclStringStream.cxx
)

ITK_WRAP_TCL(ITKCommonTcl
  wrap_ITKCommon.cxx
  wrap_ITKUtils.cxx
  wrap_ITKBase.cxx
  wrap_ITKFilterBase.cxx
  wrap_ITKRegions.cxx
  wrap_itkEventObject.cxx
  wrap_itkImage.cxx
  wrap_itkFiniteDifferenceImageFilter.cxx
  wrap_itkDenseFiniteDifferenceImageFilter.cxx
)

TARGET_LINK_LIBRARIES(ITKCommonTcl
  ITKCommon
  ITKNumerics
  VXLNumerics
  ${CABLE_TCL_LIBRARY}
)
