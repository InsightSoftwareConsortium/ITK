set(DOCUMENTATION "This module contains classes that provide an
interface between ITK and VTK.")

itk_module(ITKVtkGlue
  ENABLE_SHARED
  DEPENDS
    ITKCommon
    ITKVTK
  COMPILE_DEPENDS
    ITKImageIntensity
  TEST_DEPENDS
    ITKTestKernel
    ITKVTK
    ITKSmoothing
    ITKLabelMap
    ITKImageFusion
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION
    "${DOCUMENTATION}")

# extra test dependency on Smoothing is introduced by itkVtkMedianImagefilterTest.
# extra test dependency on ImageCompose is introduced by QuickViewTest.
# extra test dependency on LabelMap and ImageFusion is introduced by itkVtkConnectedComponentImageFilterTest
