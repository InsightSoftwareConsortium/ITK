WRAP_LIBRARY(Filtering)

set(WRAPPER_LIBRARY_GROUPS
  itkRecursiveSeparableImageFilter
  itkHessianRecursiveGaussianImageFilter
)

set(WRAPPER_LIBRARY_DEPENDS VXLNumerics Base)

set(WRAPPER_LIBRARY_LINK_LIBRARIES ITKCommon)

AUTO_INCLUDE_MODULES()
END_WRAP_LIBRARY()
