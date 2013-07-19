set(DOCUMENTATION "This module contains classes that read Philips
REC/PAR image files.")

itk_module(ITKIOPhilipsREC
  ENABLE_SHARED
  DEPENDS
    ITKIOImageBase
    ITKZLIB
  TEST_DEPENDS
    ITKTestKernel
    ITKTransform
    ITKImageGrid
    ITKImageIntensity
  EXCLUDE_FROM_ALL
  DESCRIPTION
    "${DOCUMENTATION}")
# Extra dependency of ITKTransform is introduced by itkPhilipsRECImageIOOrientationTest.
# Extra dependency of ITKImageGrid is introduced by itkPhilipsRECImageIOOrientationTest.
# Extra dependency of ITKImageIntensity is introduced by itkPhilipsRECImageIOOrientationTest.
