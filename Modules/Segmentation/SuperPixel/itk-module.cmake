set(DOCUMENTATTION "This modules contains classes related to
superpixel segmentation and clustering algorithms.")


# define the dependencies of the include module and the tests
itk_module(ITKSuperPixel
  DEPENDS
    ITKCommon
    ITKStatistics
    ITKImageGrid
  COMPILE_DEPENDS
    ITKImageSources
  TEST_DEPENDS
    ITKTestKernel
    ITKGoogleTest
    ITKMetaIO
  DESCRIPTION
    "${DOCUMENTATION}"
)
