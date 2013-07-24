set(DOCUMENTATION "This module contains ImageIO classes for reading medical
images produced by General Electric (GE) scanners.  In particular, it has
classes for the GE4, GE5, and GEAdw scanners.")

itk_module(ITKIOGE
  ENABLE_SHARED
  DEPENDS
    ITKIOIPL
  TEST_DEPENDS
    ITKTestKernel
    ITKIOSiemens
  DESCRIPTION
    "${DOCUMENTATION}"
)
