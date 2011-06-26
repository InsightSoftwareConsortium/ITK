set(DOCUMENTATION "This module contains classes that provide an
interface between ITK and VTK.")

itk_module(ITK-ItkVtkGlue
  DEPENDS
    ITK-Common
    ITK-ImageIntensity
    ITK-ImageAdaptors
    ITK-ImageGrid
    ITK-VTK
  TEST_DEPENDS
    ITK-TestKernel
    ITK-Smoothing
    ITK-ImageCompose
  EXCLUDE_FROM_ALL
  DESCRIPTION
    "${DOCUMENTATION}")

# extra test dependency on Smoothing is introduced by itkVtkMedianImagefilterTest.
# extra test dependency on ImageCompose is introduced by QuickViewTest.
