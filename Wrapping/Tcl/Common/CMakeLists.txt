INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Wrapping/Tcl/Common)

ADD_LIBRARY(ITKCommonTcl
  itkTclCommand.cxx
  itkTclStringStream.cxx
)

ITK_WRAP_TCL(ITKCommonTcl
  wrap_ITKBase.cxx
  wrap_ITKCommon.cxx
  wrap_ITKFilterBase.cxx
  wrap_ITKInterpolators.cxx
  wrap_ITKRegions.cxx
  wrap_ITKTransforms.cxx
  wrap_ITKUtils.cxx
  wrap_itkArray.cxx
  wrap_itkContinuousIndex.cxx
  wrap_itkDenseFiniteDifferenceImageFilter.cxx
  wrap_itkDifferenceImageFilter.cxx
  wrap_itkEventObject.cxx
  wrap_itkFiniteDifferenceImageFilter.cxx
  wrap_itkFixedArray.cxx
  wrap_itkImage.cxx
  wrap_itkIndex.cxx
  wrap_itkPoint.cxx
  wrap_itkSize.cxx
  wrap_itkVector.cxx
)

TARGET_LINK_LIBRARIES(ITKCommonTcl ITKCommon ${CABLE_TCL_LIBRARY})
INSTALL_TARGETS(/lib/InsightToolkit ITKCommonTcl)
