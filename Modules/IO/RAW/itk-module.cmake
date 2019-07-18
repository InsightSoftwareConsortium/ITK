set(DOCUMENTATION "This modules contains a class for reading and writing raw
binary images.  Unlike other file format readers, it is necessary to specify
critical information like the pixel type, dimensions, spacing, origin, etc. when
reading RAW files.")

itk_module(ITKIORAW
  ENABLE_SHARED
  DEPENDS
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
