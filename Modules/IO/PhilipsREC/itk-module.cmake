set(DOCUMENTATION "This module contains classes that read Philips
REC/PAR image files.")

itk_module(ITK-IO-PhilipsREC
  DEPENDS
    ITK-IO-Base
    ITK-ZLIB
  TEST_DEPENDS
    ITK-TestKernel
    ITK-Transform
    ITK-ImageGrid
    ITK-ImageIntensity
  EXCLUDE_FROM_ALL
  DESCRIPTION
    "${DOCUMENTATION}")
# Extra dependency of ITK-Transform is introduced by itkPhilipsRECImageIOOrientationTest.
# Extra dependency of ITK-ImageGrid is introduced by itkPhilipsRECImageIOOrientationTest.
# Extra dependency of ITK-ImageIntensity is introduced by itkPhilipsRECImageIOOrientationTest.
