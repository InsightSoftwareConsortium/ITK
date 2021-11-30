set(DOCUMENTATION "This module groups transformations of pixel values due to
image transform. The trivial case is the identity transformation for scalar
images. But typical non-trivial cases are the transformation of images of
geometric (contravariant) vectors, covariant vectors, or any more complex
tensor. These pixel transformations do not replace the ImageTransform but
complement it. They have been introduced to be used by itkResampleImageFilter
when the pixel type requires more complex transform than simply relocating it.")

itk_module(ITKPixelTransformation
  COMPILE_DEPENDS
    ITKImageFunction
  TEST_DEPENDS
  DESCRIPTION
    "${DOCUMENTATION}"
)

# ITKIOImageBase dependency introduced by itkResampleImageFilter.
