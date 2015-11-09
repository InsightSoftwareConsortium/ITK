set(DOCUMENTATION "Image Adaptors are classes that can take an itk::Image and
present it as a different type of image. They are commonly used for trivial
pixel-wise operations that do not justify the memory duplication that a normal
ITK filter may imply. They are also very useful for implementing casting
operations on the fly.")

itk_module(ITKImageAdaptors
  PRIVATE_DEPENDS
    ITKCommon
  TEST_DEPENDS
    ITKTestKernel
    ITKCommon
  DESCRIPTION
  "${DOCUMENTATION}"
)
