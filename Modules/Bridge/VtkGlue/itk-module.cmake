set(DOCUMENTATION "This module contains classes that provide an
interface between ITK and VTK.")

itk_module(ITKVtkGlue
  ENABLE_SHARED
  DEPENDS
    ITKCommon
    ITKVTK
  COMPILE_DEPENDS
    ITKImageIntensity
    ITKImageAdaptors
    ITKImageGrid
  TEST_DEPENDS
    ITKTestKernel
    ITKSmoothing
    ITKImageCompose
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION
    "${DOCUMENTATION}")

# extra test dependency on Smoothing is introduced by itkVtkMedianImagefilterTest.
# extra test dependency on ImageCompose is introduced by QuickViewTest.
