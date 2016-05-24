set(DOCUMENTATION "The basic idea of this generic interpolator for label images is to interpolate each label with an ordinary image interpolator, and return the label with the highest value. This is the idea used by the itk::LabelImageGaussianInterpolateImageFunction interpolator. Unfortunately, this class is currently limited to Gaussian interpolation. Using generic programming, our proposed interpolator extends this idea to any image interpolator. Combined with linear interpolation, this results in similar or better accuracy and much improved computation speeds on a test image.

A more detailed description can be found in the Insight Journal article::

Schaerer, J., Roche, F., Belaroussi, B. \"A generic interpolator for multi-label images\".
  http://hdl.handle.net/10380/3506
  http://www.insight-journal.org/browse/publication/950
  December, 2014.
")

itk_module(GenericLabelInterpolator
  DEPENDS
    ITKSmoothing
    ITKImageAdaptors
  TEST_DEPENDS
    ITKTestKernel
    ITKImageGrid
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION
    "${DOCUMENTATION}"
)
