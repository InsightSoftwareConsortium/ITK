INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Wrapping/Tcl/BasicFilters)
INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Code/BasicFilters)

ADD_LIBRARY(ITKBasicFiltersTcl)

ITK_WRAP_TCL(ITKBasicFiltersTcl
  wrap_ITKBasicFilters.cxx
  wrap_itkAnisotropicDiffusionImageFilter.cxx
  wrap_itkBinaryThresholdImageFilter.cxx
  wrap_itkCannyEdgeDetectionImageFilter.cxx
  wrap_itkCastImageFilter.cxx
  wrap_itkConfidenceConnectedImageFilter.cxx
  wrap_itkConnectedThresholdImageFilter.cxx
  wrap_itkCurvatureAnisotropicDiffusionImageFilter.cxx
  wrap_itkGradientAnisotropicDiffusionImageFilter.cxx
  wrap_itkLaplacianImageFilter.cxx
  wrap_itkMinimumMaximumImageCalculator.cxx
  wrap_itkNaryAddImageFilter.cxx
  wrap_itkRandomImageSource.cxx
  wrap_itkRecursiveGaussianImageFilter.cxx
  wrap_itkRecursiveSeparableImageFilter.cxx
  wrap_itkResampleImageFilter.cxx
  wrap_itkRescaleIntensityImageFilter.cxx
  wrap_itkShiftScaleImageFilter.cxx
  wrap_itkStatisticsImageFilter.cxx
  wrap_itkSobelEdgeDetectionImageFilter.cxx
  wrap_itkTernaryMagnitudeImageFilter.cxx
)

TARGET_LINK_LIBRARIES(ITKBasicFiltersTcl ITKBasicFilters ${CABLE_TCL_LIBRARY})
INSTALL_TARGETS(/lib/InsightToolkit ITKBasicFiltersTcl)
