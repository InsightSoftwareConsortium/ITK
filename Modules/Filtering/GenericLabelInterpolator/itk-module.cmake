set(
  DOCUMENTATION
  "This module provides a generic interpolator for multi-label images:
itkLabelImageGenericInterpolateImageFunction interpolates each label
independently with any underlying ordinary image interpolator and
returns the label with the highest interpolated value at each
point.  Generalizes the Gaussian-only behavior of
itkLabelImageGaussianInterpolateImageFunction.  See the Doxygen on
\\\\ref LabelImageGenericInterpolateImageFunction for the algorithm
and the Insight Journal article (Schaerer, Roche, Belaroussi, 2014,
https://hdl.handle.net/10380/3506) for derivation, and the module
README for in-tree vs archived-upstream scope."
)

itk_module(
  GenericLabelInterpolator
  DEPENDS
    ITKSmoothing
    ITKImageAdaptors
  TEST_DEPENDS
    ITKTestKernel
    ITKImageGrid
    ITKImageFunction
    ITKTransform
    ITKIOImageBase
    ITKIONIFTI
  DESCRIPTION "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
  ENABLE_SHARED
)
