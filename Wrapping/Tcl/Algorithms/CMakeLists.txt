INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Code/BasicFilters)
INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Code/Algorithms)

ADD_LIBRARY(ITKAlgorithmsTcl SHARED)
CABLE_CLASS_SET(Scalar float)
CABLE_CLASS_SET(Dimension 2 3)
CABLE_CLASS_SET(Image "itk::Image<$Scalar,$Dimension>")
CABLE_CLASS_SET(Filter "itk::CurvatureFlowImageFilter<$Image, $Image >")

CABLE_WRAP_TCL(ITKAlgorithmsTcl
  $Filter
  "itk::LevelSetImageFilter<$Image >"
  "itkSmartPointer_:itk::SmartPointer<$Filter >"
)

LINK_LIBRARIES(
ITKCommon
ITKBasicFilters
VXLNumerics
${CABLE_TCL_LIBRARY}
${TCL_LIBRARY}
)
